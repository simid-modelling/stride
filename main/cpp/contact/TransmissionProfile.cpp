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
 *  Copyright 2019, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Implementation for the TransmissionProfile class.
 */

#include "TransmissionProfile.h"
#include "util/StringUtils.h"

#include <cmath>

namespace stride {

using namespace std;
using namespace boost::property_tree;
using namespace stride::util;

void TransmissionProfile::Initialize(const ptree& configPt, const ptree& diseasePt)
{

    // 1. setup general transmission aspects
    m_rel_transmission_asymptomatic   = diseasePt.get<double>("disease.rel_transmission_asymptomatic",1);
    m_rel_susceptibility_children     = diseasePt.get<double>("disease.rel_susceptibility_children",1);

    // 2. setup transmission probability: with a given R0 or by age

    // parse config file (set to 0 if missing )
	const double r0 = configPt.get<double>("run.r0",0);
	const std::string transmission_age = configPt.get<std::string>("run.disease_transmission_age","");

	// if R0 is -1 (=not used) and the config contains age-specific transmission probabilities
	if(r0 == -1 && transmission_age.length()>0){


//        	// 1. using values from the the disease file
//			for (unsigned int index_age = 0; index_age < m_transmission_probability_age.size(); index_age++) {
//					auto transmissionRate = configPt.get<double>("disease.transmission.age" + std::to_string(index_age),0);
//					m_transmission_probability_age[index_age] = transmissionRate;
//			}

		// 2. using values from the main configuration file
		auto transmission_rate_string = Split(configPt.get<std::string>("run.disease_transmission_age"),",");
		for (unsigned int index_age = 0; index_age < m_transmission_probability_age.size(); index_age++) {

			if(index_age < transmission_rate_string.size()){
				m_transmission_probability_age[index_age] = stod(transmission_rate_string[index_age]);;
			} else{
				m_transmission_probability_age[index_age] = 0;
			}
		}


	} else { // default behavior using the given R0

		double transmission_probability = 0;
		const auto b0 = diseasePt.get<double>("disease.transmission.b0");
		const auto b1 = diseasePt.get<double>("disease.transmission.b1");
		const auto b2 = diseasePt.get<double>("disease.transmission.b2");

		// if linear model is fitted to simulation data:
		if(b2 == 0){
			// Expected(R0) = (b0 + b1*transm_prob).
			transmission_probability = (r0 - b0) / b1;

		} else{
		// if quadratic model is fitted to simulation data:
			// Expected(R0) = (b0 + b1*transm_prob + b2*transm_prob^2).
			// Find root
			const auto a = b2;
			const auto b = b1;
			const auto c = b0 - r0;

			// To obtain a real values (instead of complex)
			if (r0 < (-(b * b) / (4 * a))) {
					const double determ = (b * b) - 4 * a * c;
					transmission_probability = (-b + sqrt(determ)) / (2 * a);
			} else {
					throw runtime_error("TransmissionProfile::Initialize> Illegal input values.");
			}
		}

		// Code to maintain backward compatibility
		for (unsigned int index_age = 0; index_age < m_transmission_probability_age.size(); index_age++) {
			m_transmission_probability_age[index_age] = transmission_probability;
		}

	}
}

} // namespace stride
