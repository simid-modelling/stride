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
#include "util/Containers.h"
#include "util/CSV.h"
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
                m_unitest_planning_output_fn(),
	        m_unitest_pool_allocation(),
		m_unitest_fnr(-1.0), m_unitest_n_tests_per_day(0), m_unitest_pool_size(0),
	        m_unitest_test_compliance(0.0), m_unitest_isolation_compliance(0.0),
                m_unitest_isolation_delay(0),
	        m_unitest_planning(),
	        m_unitest_day_in_sweep(0),
		m_tracing_efficiency_household(0),m_tracing_efficiency_other(0),m_case_finding_capacity(0),m_delay_isolation_index(0),m_delay_contact_tracing(0),
		m_test_false_negative(0),  m_school_system_adjusted(false)
	{}

void PublicHealthAgency::Initialize(const ptree& config){
	m_telework_probability        = config.get<double>("run.telework_probability",0);
	m_detection_probability       = config.get<double>("run.detection_probability",0);

	m_unitest_pool_allocation      = config.get<std::string>("run.unitest_pool_allocation","");
        m_unitest_fnr                  = config.get<double>("run.unitest_fnr",-1.0);
        m_unitest_n_tests_per_day      = config.get<unsigned int>("run.unitest_n_tests_per_day",0);
        m_unitest_pool_size            = config.get<unsigned int>("run.unitest_pool_size",0);
        m_unitest_test_compliance      = config.get<double>("run.unitest_test_compliance",0.0);
        m_unitest_isolation_compliance = config.get<double>("run.unitest_isolation_compliance",0.0);
        m_unitest_isolation_delay      = config.get<int>("run.unitest_isolation_delay",1);
        const auto prefix = config.get<string>("run.output_prefix");
        m_unitest_planning_output_fn   = FileSys::BuildPath(prefix, "unitest_planning.csv");

        Replace(m_unitest_pool_allocation, "$unitest_pool_size", std::to_string(m_unitest_pool_size));
	
	m_tracing_efficiency_household = config.get<double>("run.tracing_efficiency_household",0);
	m_tracing_efficiency_other     = config.get<double>("run.tracing_efficiency_other",0);

	m_case_finding_capacity  = config.get<unsigned int>("run.case_finding_capacity",0);

	m_delay_isolation_index          = config.get<unsigned int>("run.delay_isolation_index",3);
	m_delay_contact_tracing  = config.get<unsigned int>("run.delay_contact_tracing",1);
	m_test_false_negative    = config.get<double>("run.test_false_negative",0.3);

	m_school_system_adjusted = config.get<unsigned int>("run.school_system_adjusted",0) == 1;

	// account for false negative tests
	m_detection_probability        *= (1.0 - m_test_false_negative);
	m_tracing_efficiency_household  *= (1.0 - m_test_false_negative);
	m_tracing_efficiency_other      *= (1.0 - m_test_false_negative);

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

bool PublicHealthAgency::Bernoulli(std::function<double()> uniform_01_rng, double prob_of_success)
{
  return uniform_01_rng() < prob_of_success; 
}

void PublicHealthAgency::PerformUniversalTesting(std::shared_ptr<Population> pop, util::RnMan& rnMan,
                                            unsigned short int simDay)
{
  if (m_unitest_fnr < 0.0)
    return;

  if (m_unitest_planning.empty()) {
    const auto& households = pop->CRefPoolSys().CRefPools(Id::Household);

    std::map<std::string, std::map<int, PCRPool>> pools_per_georegion;
    unsigned int total_pools = 0;
    CSV allocation(m_unitest_pool_allocation);
    size_t georegion_idx = allocation.GetIndexForLabel("province");
    size_t pool_id_idx = allocation.GetIndexForLabel("pool_id");
    size_t household_id_idx = allocation.GetIndexForLabel("household_id");
    for (const auto& row : allocation) {
        std::string georegion = row.GetValue(georegion_idx);
        int pool_id = row.GetValue<int>(pool_id_idx);

        //if the georegion is not yet in the map, introduce it 
        if (pools_per_georegion.find(georegion) == pools_per_georegion.end()) {
            pools_per_georegion[georegion] = std::map<int,PCRPool>();
        }
        auto& pools = pools_per_georegion[georegion];

        //if the PCR pool is not yet in the map, introduce it 
        if (pools.find(pool_id) == pools.end()) {
            pools[pool_id] = PCRPool();
            pools[pool_id].SetId(pool_id);
            pools[pool_id].SetGeoRegion(georegion);

            ++total_pools;
        }
        auto& pool = pools[pool_id];

        int household_id = row.GetValue<int>(household_id_idx);
		const auto& household = households[household_id].GetPool();
        pool.AddHousehold(household);
    } 
    
    unsigned int n_days = ceil(total_pools / (float)m_unitest_n_tests_per_day);
    for (unsigned int day = 0; day < n_days; ++day) {
        m_unitest_planning.push_back(std::set<PCRPool>());
    }

    unsigned int day = 0;
    //TODO: shuffle regions keys randomly
    for (const auto& key_val: pools_per_georegion) {
        const auto& region = key_val.first;
        const auto& pools = util::MapValuesToVec(pools_per_georegion[region]);
        //TODO: pools can be shuffled randomly
        for (const auto& pool: pools) {
            m_unitest_planning[day].insert(pool);
            ++day;
            //when at the end of the planning, reset day
            if (day == m_unitest_planning.size())
               day = 0; 
        }
    }

    //write the planning to file
    ofstream of;
    of.open(m_unitest_planning_output_fn.c_str());
    of << "day,georegion,id,size" << std::endl;
    for (unsigned int day = 0; day < n_days; ++day) {
        for (const auto& pool: m_unitest_planning[day]) {
            of << day << "," 
               << pool.GetGeoRegion() << "," 
               << pool.GetId() << ","
               << pool.GetIndividuals().size()
               << std::endl;
        }
    }
    of.close();

#ifndef NDEBUG
    unsigned int enlisted_pop = 0;
    //test: check that the stride population and the population allocated over pools coincide
    for (unsigned int day = 0; day < n_days; ++day) {
        for (const auto& pool : m_unitest_planning[day]) {
            enlisted_pop += pool.GetIndividuals().size();
        }
    }
    std::cerr << "[UNIVERSAL] Enlisted vs total pop:"
        << enlisted_pop << " vs " << pop->size() << std::endl;
    assert(enlisted_pop == pop->size());

    //test: check that the pools' size does not exceed m_unitest_pool_size
    //test: report the nr of pools that are not completely full 
    //        (i.e., leftover_pools, there should not be many of them)
    int leftover_pools = 0;
    int filled_pools = 0;
    for (unsigned int day = 0; day < n_days; ++day) {
        for (const auto& pool : m_unitest_planning[day]) {
            assert(pool.GetIndividuals().size() <= m_unitest_pool_size);
            if (pool.GetIndividuals().size() < m_unitest_pool_size) {
                leftover_pools += 1;
            } else {
                filled_pools += 1;
            }
        }
    }
    std::cerr << "[UNIVERSAL] Leftover pools: " << leftover_pools << std::endl;
    std::cerr << "[UNIVERSAL] Filled pools: " << filled_pools << std::endl;

    //test: verify that the number of daily tests is not exceeded
    for (unsigned int day = 0; day < n_days; ++day) {
        std::cerr << "[UNIVERSAL]"
            << " Daily tests " << m_unitest_planning[day].size()
            << " on day " << day
            << " vs budget " <<  m_unitest_n_tests_per_day << std::endl;
        assert(m_unitest_planning[day].size() <= m_unitest_n_tests_per_day);
    }
#endif 
  }

  auto& logger = pop->RefEventLogger();
  auto uniform01Gen = rnMan.GetUniform01Generator(0U);

  //perform the testing, according to the planning
  for (const auto& pool : m_unitest_planning[m_unitest_day_in_sweep]) {
    bool pool_positive = false;
    std::vector<std::vector<Person*>> tested_households;
    for (const auto& household : pool.GetHouseholds()) {
      bool compliant = Bernoulli(uniform01Gen, m_unitest_test_compliance);
     
      if (compliant) {
        tested_households.push_back(household);
      
        for (const Person* indiv : household) {
          if (indiv->GetHealth().IsInfected())
            pool_positive = true;
        }
      }
    }

    bool pcr_test_positive = pool_positive && Bernoulli(uniform01Gen, 1-m_unitest_fnr);
    if (pcr_test_positive) {
      for (auto& household : tested_households) {
        bool isolation_compliance = Bernoulli(uniform01Gen, m_unitest_isolation_compliance);
        if (isolation_compliance) {
          for (const auto& indiv : household) {
              indiv->GetHealth().StartIsolation(m_unitest_isolation_delay);
              logger->info("[UNITEST-ISOLATE] {} {} {} {} {} {}",
                                                 pool.GetId(),
                                                 indiv->GetPoolId(Id::Household),
                                                 indiv->GetId(), 
                                                 indiv->GetHealth().IsInfected(),
                                                 m_unitest_isolation_delay,
                                                 simDay);
          }
        }
      }
    }
  }

  //move to the next day in the sweep,
  //if at the end of the sweep: reset
  ++m_unitest_day_in_sweep;
  if (m_unitest_day_in_sweep == m_unitest_planning.size()) {
    m_unitest_day_in_sweep = 0; 
  }
}

bool PublicHealthAgency::IsContactTracingActive(const std::shared_ptr<Calendar> calendar) const {
	return (m_detection_probability > 0) && calendar->IsContactTracingActivated();
}


void PublicHealthAgency::PerformContactTracing(std::shared_ptr<Population> pop, ContactHandler& cHandler,
												const std::shared_ptr<Calendar> calendar)
{

	// if contact tracing not active, stop
	if (!IsContactTracingActive(calendar)) {
			return;
	}

	//cout << m_detection_probability << " -- "<< m_tracing_efficiency_household << " -- "<< m_tracing_efficiency_other << " ** " << m_case_finding_capacity << endl;

	auto& logger       = pop->RefEventLogger();
	const auto  simDay = calendar->GetSimulationDay();

	/// Mark index cases for track&trace
	for (auto& p_case : *pop) {

		if(p_case.GetHealth().NumberDaysInfected(1) &&
				cHandler() < m_detection_probability) {
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
				double tracing_efficiency = m_tracing_efficiency_other;

				// set default poolType as "other
				std::string poolTypeString = "School";

				// if contact is part of same household, change tracing efficiency and poolType
				if(p_contact->GetPoolId(Id::Household) == p_case.GetPoolId(Id::Household)){
					poolTypeString    = ToString(Id::Household);
					tracing_efficiency = m_tracing_efficiency_household;
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

				if(cHandler() < tracing_efficiency){

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
