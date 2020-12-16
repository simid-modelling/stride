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
 *  Copyright 2019, Willem L, Kuylen E, Broeckhove J
 */

/**
 * @file
 * Header for the TransmissionProfile class.
 */

#pragma once

#include <boost/property_tree/ptree.hpp>
#include <vector>
#include <numeric>

#include "pop/Person.h"

namespace stride {

/**
 * Transmission probability from disease data.
 */
class TransmissionProfile
{
public:
        /// Initialize.
        TransmissionProfile() : m_transmission_probability_age(100) {
        }

        /// Return average transmission probability (not weighted).
        double GetProbability() const {
        	double transmission_probability_mean = accumulate(m_transmission_probability_age.begin(),
        													  m_transmission_probability_age.end(),
															  0.0) / m_transmission_probability_age.size();
        	return transmission_probability_mean;
        }

        /// Return age-specific transmission probability.
        double GetProbability(unsigned int age) const {
          	if(age < m_transmission_probability_age.size()){
        		return m_transmission_probability_age[age];
        	} else{
        		return m_transmission_probability_age[m_transmission_probability_age.size()-1];
        	}
        }

        /// Return age- and health-specific transmission probability.
		double GetProbability(Person* p_infected, Person* p_susceptible) const {

			// infector specific transmission probability
			double probability_infected    = (p_infected->GetHealth().IsSymptomatic()) ? 1 : m_rel_transmission_asymptomatic;

			// deprecated... this binary option will be removed in the future
			double probability_susceptible_child = (p_susceptible->GetAge() < 18) ? m_rel_susceptibility_children : 1;

			// get age-specific transmission value based on the age of the susceptible individual
			double probability_susceptible = GetProbability(p_susceptible->GetAge());


			// return combination
			return probability_infected * probability_susceptible_child * probability_susceptible;
		}

        /// Initialize.
        void Initialize(const boost::property_tree::ptree& configPt, const boost::property_tree::ptree& diseasePt);

private:
        std::vector<double> m_transmission_probability_age;

        double             m_rel_transmission_asymptomatic;	   ///< Relative reduction of transmission for asymptomatic cases
        double             m_rel_susceptibility_children;	   ///< Relative reduction of susceptibility for children vs. adults

};

} // namespace stride
