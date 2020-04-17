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

#include "contact/ContactPool.h"
#include "util/RnMan.h"
#include "util/SegmentedVector.h"

#include <boost/property_tree/ptree_fwd.hpp>
#include <memory>

namespace stride {

class Population;
class Sim;

/**
 * Sets intervention measures.
 */
class PublicHealthAgency
{
public:
	    /// Default constructor
	    PublicHealthAgency(){};

        /// Initializing PublicHealthAgency.
		void Initialize(const boost::property_tree::ptree& config);

        /// set telework features.
        void SetTelework(std::shared_ptr<Population> pop, util::RnMan& rnMan);

        /// Public Health Strategy: look for contacts of infected cases and quarantine infected cases
		void PerformContactTracing(std::shared_ptr<Population> pop, util::RnMan& rnMan, unsigned short int simDay);

private:
        double m_telework_probability;    ///< Probability to perform telework (or equivalent) //TODO rename "telework"
        double m_detection_probability;   ///< Detection probability of symptomatic cases.
        double m_case_finding_efficency;  ///< Detection probability of infected cases during case finding

};

} // namespace stride
