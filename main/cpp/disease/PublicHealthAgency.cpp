/*
 *  This is free software: you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *  The software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  You should have received a copy of the GNU General Public License
 *  along with the software. If not, see <http://www.gnu.org/licenses/>.
 *
 *  Copyright 2020, Willem L
 */

/**
 * @file
 * Implementation for the PublicHealthAgency class.
 */

#include "PublicHealthAgency.h"

#include "calendar/Calendar.h"
#include "pop/Population.h"
#include "util/FileSys.h"
#include "util/LogUtils.h"
#include "util/StringUtils.h"

#include <boost/property_tree/ptree.hpp>
#include <algorithm>

namespace stride {

using namespace boost::property_tree;
using namespace stride::ContactType;
using namespace stride::util;
using namespace std;

// Default constructor
PublicHealthAgency::PublicHealthAgency(): m_telework_probability(0),m_detection_probability(0),
		m_tracing_efficency_household(0),m_tracing_efficency_other(0),m_case_finding_capacity(0),m_delay_isolation_index(0),m_delay_contact_tracing(0),
		m_test_false_negative(0),  m_school_system_adjusted(false)
	{}

void PublicHealthAgency::Initialize(const ptree& config){
	m_telework_probability        = config.get<double>("run.telework_probability",0);
	m_detection_probability       = config.get<double>("run.detection_probability",0);
	m_tracing_efficency_household = config.get<double>("run.tracing_efficency_household",0);
	m_tracing_efficency_other     = config.get<double>("run.tracing_efficency_other",0);

	m_case_finding_capacity  = config.get<unsigned int>("run.case_finding_capacity",0);

	m_delay_isolation_index          = config.get<unsigned int>("run.delay_isolation_index",3);
	m_delay_contact_tracing  = config.get<unsigned int>("run.delay_contact_tracing",1);
	m_test_false_negative    = config.get<double>("run.test_false_negative",0.3);

	m_school_system_adjusted = config.get<unsigned int>("run.school_system_adjusted",0) == 1;

	// account for false negative tests
	m_detection_probability        *= (1.0 - m_test_false_negative);
	m_tracing_efficency_household  *= (1.0 - m_test_false_negative);
	m_tracing_efficency_other      *= (1.0 - m_test_false_negative);

}

void PublicHealthAgency::SetTelework(std::shared_ptr<Population> pop, util::RnMan& rnMan)
{
	auto       generator   = rnMan.GetUniform01Generator();
	for (auto& p : *pop) {
		if(p.GetPoolId(ContactType::Id::Workplace) != 0 &&  generator() < m_telework_probability){
			p.SetTeleworkAbility();
		}
	}
}

bool PublicHealthAgency::IsK12SchoolOff(unsigned int age, bool isPreSchoolOff,
		bool isPrimarySchoolOff, bool isSecondarySchoolOff, bool isCollegeOff){

	if(isPreSchoolOff && isPrimarySchoolOff && isSecondarySchoolOff){
		return true;
	}

	// apply adjusted scheme based on covid-19 exit strategies?
	if(m_school_system_adjusted){

		// note: use school types to differentiate in timing, with specific ages
		// "pre-school"       => 4 days/week: 1th and 2th year primary school + children in daycare (0-2y)
		// "primary school"   => 2 days/week: 6th of primary school
		// "secondary school" => 1 day/week:  6th of secondary school
		// "college"          => stay closed

		if(!isPreSchoolOff && age <= 2) { return false; }
		if(!isPreSchoolOff && age == 6)  { return false; }
		if(!isPreSchoolOff && age == 7)  { return false; }

		if(!isPrimarySchoolOff && age == 11) { return false; }

		if(!isSecondarySchoolOff && age == 17) { return false; }

		if(!isCollegeOff) { return false; }


	} else {// note: use school types to differentiate in timing, with regular age groups
			if(!isPreSchoolOff && age < 6)                     {  return false; } //TODO : fix hardcoded age intervals
			if(!isPrimarySchoolOff && age < 12 && age >= 6)    {  return false; }
			if(!isSecondarySchoolOff && age < 18 && age >= 12) {  return false; }
	}

	return true;



}

bool PublicHealthAgency::IsContactTracingActive(const std::shared_ptr<Calendar> calendar) const {

	return (m_detection_probability > 0) && calendar->IsContactTracingActivated();

}


void PublicHealthAgency::PerformContactTracing(std::shared_ptr<Population> pop, util::RnMan& rnMan,
												const std::shared_ptr<Calendar> calendar)
{

	// if contact tracing not active, stop
	if (!IsContactTracingActive(calendar)) {
			return;
	}

	//cout << m_detection_probability << " -- "<< m_tracing_efficency_household << " -- "<< m_tracing_efficency_other << " ** " << m_case_finding_capacity << endl;

	auto  uniform01Gen = rnMan.GetUniform01Generator(0U);
	auto& logger       = pop->RefEventLogger();
	const auto  simDay = calendar->GetSimulationDay();

	/// Mark index cases for track&trace
	for (auto& p_case : *pop) {

		if(p_case.GetHealth().NumberDaysInfected(1) &&
				uniform01Gen() < m_detection_probability) {
			p_case.SetTracingIndexCase();
		}

	}

	/// Set counter for index cases
	unsigned int num_index_cases = 0;

	/// Loop over the population to find index cases on day X after symptom onset
	for (auto& p_case : *pop) {

		if (p_case.IsTracingIndexCase() && p_case.GetHealth().NumberDaysSymptomatic(m_delay_isolation_index)	) {

			// set index case in quarantine
			p_case.GetHealth().StartIsolation(0);

			// counter for number of contacts tested
			unsigned int num_contacts_tested = 0;

			// get contact register and iterator
			vector<Person*> cnt_register = p_case.GetContactRegister();
			vector<Person*>::iterator ip;

			// cout << p_case.GetId() << ": size (orig) " <<  cnt_register.size() << endl;

			// Sorting the vector, get the unique elements, and remove the others
			std::sort(cnt_register.begin(), cnt_register.end());
			ip = std::unique(cnt_register.begin(), cnt_register.end());
			cnt_register.resize(std::distance(cnt_register.begin(), ip));

			// cout << p_case.GetId() << ": size (unique) " <<  cnt_register.size() << endl;

			// loop over contact register
			for (ip = cnt_register.begin(); ip != cnt_register.end(); ++ip) {


				Person* p_contact = *ip;

				// combine contact tracing efficiency and false negative rate
				double tracing_efficency = m_tracing_efficency_other;

				// set default poolType as "other
				std::string poolTypeString = "School";

				// if contact is part of same household, change tracing efficiency and poolType
				if(p_contact->GetPoolId(Id::Household) == p_case.GetPoolId(Id::Household)){
					poolTypeString    = ToString(Id::Household);
					tracing_efficency = m_tracing_efficency_household;
				}

				// if contact is part of same community, change poolType
				if(p_contact->GetPoolId(Id::PrimaryCommunity) == p_case.GetPoolId(Id::PrimaryCommunity)||
						p_contact->GetPoolId(Id::SecondaryCommunity) == p_case.GetPoolId(Id::SecondaryCommunity)){
					poolTypeString    = "Community";
				}

				// if contact is part of same workplace, change poolType
				if(p_case.GetPoolId(Id::Workplace) != 0 &&
						p_contact->GetPoolId(Id::Workplace) == p_case.GetPoolId(Id::Workplace)){
					poolTypeString    = "Workplace";
				}

				if(uniform01Gen() < tracing_efficency){

					if(p_contact->GetHealth().IsInfected()){
						// start isolation over X days
						p_contact->GetHealth().StartIsolation(m_delay_contact_tracing);

						// add to log (TODO: check log_level)
						logger->info("[TRACE] {} {} {} {} {} {} {} {} {} {}",
								p_contact->GetId(), p_contact->GetAge(),
								p_contact->GetHealth().IsInfected(),
								p_contact->GetHealth().IsSymptomatic(),
								poolTypeString, p_case.GetId(),
								p_case.GetAge(), simDay, -1, -1);
					}
					// increment contact counter
					num_contacts_tested++;
				}
			}

			// Log index case
			// TODO: check log_level
			logger->info("[TRACE] {} {} {} {} {} {} {} {} {} {}",
						 p_case.GetId(), p_case.GetAge(),
						 p_case.GetHealth().IsInfected(),
						 p_case.GetHealth().IsSymptomatic(),
						 "Index", p_case.GetId(),
						 p_case.GetAge(), simDay, cnt_register.size(), num_contacts_tested);

			// update index case counter, and terminate if quota is reached
			num_index_cases++;
			if(num_index_cases >= m_case_finding_capacity){
				return;
			}
		}
	}
}


} // namespace stride
