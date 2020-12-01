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

        /// Initialize.
        void Initialize(const boost::property_tree::ptree& configPt, const boost::property_tree::ptree& diseasePt);

private:
        std::vector<double> m_transmission_probability_age;
};

} // namespace stride
