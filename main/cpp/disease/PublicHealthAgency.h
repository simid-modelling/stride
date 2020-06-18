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
 * Header for the PublicHealthAgency class.
 */

#pragma once

#include "PCRPool.h"
#include "contact/ContactPool.h"
#include "util/RnMan.h"
#include "util/FileSys.h"
#include "util/SegmentedVector.h"

#include <boost/property_tree/ptree_fwd.hpp>
#include <memory>
#include <set>
#include <vector>

namespace stride {

class Calendar;
class Population;
class Sim;

/**
 * Sets intervention measures.
 */
class PublicHealthAgency
{
public:
	    /// Default constructor
	    PublicHealthAgency();

        /// Initializing PublicHealthAgency.
		void Initialize(const boost::property_tree::ptree& config);

        /// set telework features.
        void SetTelework(std::shared_ptr<Population> pop, util::RnMan& rnMan);

        /// Public Health Strategy: look for contacts of infected cases and quarantine infected cases
		void PerformContactTracing(std::shared_ptr<Population> pop, util::RnMan& rnMan, unsigned short int simDay);
       /// Public Health Strategy: perform universal testing
       void PerformUniversalTesting(std::shared_ptr<Population> pop, util::RnMan& rnMan, unsigned short int simDay);

		bool IsK12SchoolOff(unsigned int age, bool isPreSchoolOff, bool isPrimarySchoolOff, bool isSecondarySchoolOff, bool isCollegeOff);

private:
        bool Bernoulli(std::function<double()> uniform_01_rng, double prob_of_success);

private:
        double m_telework_probability;    ///< Probability to perform telework (or equivalent) //TODO rename "telework"
        filesys::path m_unitest_planning_output_fn; ///> Filename to output the planning to
        //universal testing configuration
        std::string m_unitest_pool_allocation; ///< File that lists the pool to which households belong
        double m_unitest_fnr;             ///< False negative rate for pool testing (universal testing)
        unsigned int m_unitest_n_tests_per_day; ///< Number of PCR tests per day (universal testing)
        unsigned int m_unitest_pool_size; ///< Pool size (universal testing)
        double m_unitest_test_compliance; ///< Household compliance with testing (universal testing)
        double m_unitest_isolation_compliance; ///< Household compliance when isolated (universal testing)
        unsigned int m_unitest_isolation_delay; ///< Delay (in days) after which positive individuals are isolated (universal testing)
        //universal testing planning
        std::vector<std::set<PCRPool>> m_unitest_planning; ///< Vector with at each element, a set of PCR pools, to be performed at one day
        unsigned int m_unitest_day_in_sweep; ///< The n-th day of the current universal testing sweep
        //contact tracing configuration
        double m_detection_probability;   ///< Detection probability of symptomatic cases.
        double m_case_finding_efficency;  ///< Detection probability of infected cases during case finding
        unsigned int m_case_finding_capacity;  ///< Maximum number of symptomatic cases with contact tracing per day
        unsigned int m_delay_testing;         ///< Number of days after symptom onset to perform a clinical test
        unsigned int m_delay_contact_tracing; ///< Number of days after clinical test to start contact tracing
		double m_test_false_negative;         ///< False negative rate of PCR tests
		bool m_identify_all_cases;            ///< Boolean to identify all cases in the network of the index, or only his/her secondary cases

		bool m_school_system_adjusted;         ///< Apply adjusted school system for pre-, primary and secondary schools?
};

} // namespace stride
