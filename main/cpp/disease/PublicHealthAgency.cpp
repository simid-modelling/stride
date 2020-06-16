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

namespace stride {

using namespace boost::property_tree;
using namespace stride::ContactType;
using namespace stride::util;
using namespace std;

// Default constructor
PublicHealthAgency::PublicHealthAgency(): m_telework_probability(0),
	        m_unitest_pool_allocation(),
		m_unitest_fnr(0.0), m_unitest_n_tests_per_day(0), m_unitest_pool_size(0),
	        m_unitest_test_compliance(0.0), m_unitest_isolation_compliance(0.0),
	        m_unitest_planning(),
		m_detection_probability(0),
		m_case_finding_efficency(0),m_case_finding_capacity(0),m_delay_testing(0),m_delay_contact_tracing(0),
		m_test_false_negative(0), m_identify_all_cases(false), m_school_system_adjusted(false)
	{}

void PublicHealthAgency::Initialize(const ptree& config){
        m_unitest_pool_allocation      = config.get<std::string>("run.unitest_pool_allocation","");
        m_unitest_fnr                  = config.get<double>("run.unitest_fnr",0.0);
        m_unitest_n_tests_per_day      = config.get<unsigned int>("run.unitest_n_tests_per_day",0);
        m_unitest_pool_size            = config.get<unsigned int>("run.unitest_pool_size",0);
        m_unitest_test_compliance      = config.get<double>("run.unitest_test_compliance",0.0);
        m_unitest_isolation_compliance = config.get<double>("run.unitest_isolation_compliance",0.0);
  
	m_telework_probability   = config.get<double>("run.telework_probability",0);
	m_detection_probability  = config.get<double>("run.detection_probability",0);
	m_case_finding_efficency = config.get<double>("run.case_finding_efficency",0);
	m_case_finding_capacity  = config.get<unsigned int>("run.case_finding_capacity",0);

	m_delay_testing          = config.get<unsigned int>("run.delay_testing",3);
	m_delay_contact_tracing  = config.get<unsigned int>("run.delay_contact_tracing",1);
	m_test_false_negative    = config.get<double>("run.test_false_negative",0.3);
	m_identify_all_cases     = config.get<unsigned int>("run.identify_all_cases",1) == 1;

	m_school_system_adjusted = config.get<unsigned int>("run.school_system_adjusted",0) == 1;

	// account for false negative tests
	m_detection_probability  *= (1-m_test_false_negative);
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



void PublicHealthAgency::PerformUniversalTesting(std::shared_ptr<Population> pop, util::RnMan& rnMan,
                                            unsigned short int simDay)
{
  if (m_unitest_fnr <= 0.0)
    return;

  if (m_unitest_planning.empty()) {
    const auto& households = pop->CRefPoolSys().CRefPools(Id::Household);

    std::map<std::string, std::map<int, PCRPool>> pools_per_georegion;
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
        }
        auto& pool = pools[pool_id];

        int household_id = row.GetValue<int>(household_id_idx);
		for (const auto& hh_member : households[household_id].GetPool()) {
            pool.AddIndividual(hh_member);     
        }
    } 
    
    unsigned int n_days = ceil(pop->size() / (float)(m_unitest_n_tests_per_day * m_unitest_pool_size));
    for (unsigned int day = 0; day < n_days; ++day) {
        m_unitest_planning.push_back(std::set<PCRPool>());
    }

    for (const auto& key_val: pools_per_georegion) {
        const auto& region = key_val.first;
        unsigned int pools_per_day = ceil(pools_per_georegion[region].size()/float(n_days));
        const auto& pools = util::MapValuesToVec(pools_per_georegion[region]);
        const auto& chunks = util::SplitVec(pools, pools_per_day);
        for (unsigned int day = 0; day < n_days; ++day) {
            std::vector<PCRPool> chunk = chunks[day];
            for (const auto& pool: chunk) {
                m_unitest_planning[day].insert(pool);
            } 
        }
    }
  }

#ifndef NDEBUG
  unsigned int n_days = m_unitest_planning.size();

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

void PublicHealthAgency::PerformContactTracing(std::shared_ptr<Population> pop, util::RnMan& rnMan,
                                            unsigned short int simDay)
{

//	cout << m_detection_probability << " -- "<< m_case_finding_efficency << " ** " << m_case_finding_capacity << endl;

	// perform case finding, only if the probability is > 0.0
	if (m_detection_probability <= 0.0) {
			return;
	}

	using namespace ContactType;
	auto  uniform01Gen = rnMan.GetUniform01Generator(0U);
	auto& logger       = pop->RefContactLogger();

	/// To allow iteration over pool types for the PublicHealthAgency.
	std::initializer_list<Id> AgencyPoolIdList{Id::Household, Id::Workplace, Id::K12School, Id::College, Id::HouseholdCluster};

	/// Start counting tested cases
	unsigned int num_index_cases = 0;

	for (auto& p_case : *pop) {
			if (p_case.GetHealth().SymptomsStartedDaysBefore(m_delay_testing) &&
					!p_case.GetHealth().IsInIsolation() &&
					uniform01Gen() < m_detection_probability) {

				// set case in quarantine
				p_case.GetHealth().StartIsolation(0);

				unsigned int num_potential_contacts = 0;

				// loop over his/her contacts
				for (Id typ : AgencyPoolIdList) {


					const auto& pools  = pop->CRefPoolSys().CRefPools(typ);
					const auto  poolId = p_case.GetPoolId(typ);
					if (poolId == 0) {
							continue;
					}

					//TODO: skip contact tracing if more than 50p present in this pool
					if (pools[poolId].GetPool().size() > 50) {
							continue;
					}

					for (const auto& p_member : pools[poolId].GetPool()) {
						if (p_case != *p_member &&
								(typ == Id::Household || typ == Id::HouseholdCluster ||
										uniform01Gen() < m_case_finding_efficency)) {

								// start quarantine measure if infected, no false negative and linked with index cases (optional)
								if(p_member->GetHealth().IsInfected() &&
										(m_identify_all_cases || p_member->GetHealth().GetIdInfector() == p_case.GetId()) &&
										uniform01Gen() < (1-m_test_false_negative)){

//									std::cout << "case found!!" << std::endl;
									p_member->GetHealth().StartIsolation(m_delay_contact_tracing);

									// TODO: check log_level
									logger->info("[TRACE] {} {} {} {} {} {} {} {} {} {}",
												 p_member->GetId(), p_member->GetAge(),
												 p_member->GetHealth().IsInfected(),
												 p_member->GetHealth().IsSymptomatic(),
												 ToString(typ), poolId, p_case.GetId(),
												 p_case.GetAge(), simDay, -1);
								}

								num_potential_contacts++;
						} // end if clause
				} // end for-loop: pool members
			} // end for-loop: pool types

			// TODO: check log_level
			logger->info("[TRACE] {} {} {} {} {} {} {} {} {} {}",
						 p_case.GetId(), p_case.GetAge(),
						 p_case.GetHealth().IsInfected(),
						 p_case.GetHealth().IsSymptomatic(),
						 -1, -1, p_case.GetId(),
						 p_case.GetAge(), simDay, num_potential_contacts);

			// update counter and end if limit is reached
			num_index_cases++;
			if(num_index_cases >= m_case_finding_capacity){
				return;
			}
		}
	}
}


} // namespace stride
