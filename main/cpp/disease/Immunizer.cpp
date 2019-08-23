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
 *  Copyright 2017, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Implementation for the Immunizer class.
 */

#include "Immunizer.h"

#include "pop/Person.h"
#include "util/RnMan.h"

#include <numeric>
#include <vector>

namespace stride {

using namespace std;
using namespace util;


void Immunizer::Random(const SegmentedVector<ContactPool>& pools, vector<double>& immunityDistribution,
                       double immunityLinkProbability,std::shared_ptr<Population> pop, const bool log_immunity)
{
        // Initialize a vector to count the population per age class [0-100].
        vector<double> populationBrackets(100, 0.0);

        // Sampler for int in [0, pools.size()) and for double in [0.0, 1.0).
        const auto poolsSize          = static_cast<int>(pools.size());
        auto       intGenerator       = m_rn_man.GetUniformIntGenerator(0, poolsSize, 0U);
        auto       uniform01Generator = m_rn_man.GetUniform01Generator(0U);
        auto&      logger             = pop->RefContactLogger();

        // Count susceptible individuals per age class
        for (auto& c : pools) {
                for (const auto& p : c.GetPool()) {
                        if (p->GetHealth().IsSusceptible()) {
                                populationBrackets[p->GetAge()]++;
                        }
                }
        }


        // Calculate the number of "new immune" individuals per age class.
        unsigned int numImmune = 0;
        for (unsigned int age = 0; age < 100; age++) {
        	 	auto temp_pop_num = populationBrackets[age];
                populationBrackets[age] = floor(populationBrackets[age] * immunityDistribution[age]);
                numImmune += static_cast<unsigned int>(populationBrackets[age]);
                //std::cout << age << " -- " << populationBrackets[age] << " -- " << temp_pop_num << " -- " << immunityDistribution[age] << std::endl;

        }

        // Sample immune individuals, until all age-dependent quota are reached.
        while (numImmune > 0) {
                // random pool, random order of members
                const ContactPool&   p_pool = pools[intGenerator()];
                const auto           size   = static_cast<unsigned int>(p_pool.GetPool().size());
                vector<unsigned int> indices(size);
                iota(indices.begin(), indices.end(), 0U);
                m_rn_man.Shuffle(indices, 0U);

                // loop over members, in random order
                for (unsigned int i_p = 0; i_p < size && numImmune > 0; i_p++) {
                        Person& p = *p_pool[indices[i_p]];
                        // if p is susceptible and his/her age class has not reached the quota => make immune
                        if (p.GetHealth().IsSusceptible() && populationBrackets[p.GetAge()] > 0) {
                                p.GetHealth().SetImmune();

                                populationBrackets[p.GetAge()]--;
                                numImmune--;
                                // TODO: check log_level
                                if(log_immunity){
                                	logger->info("[VACC] {} {} {} {} {} {} {} {}",
                                				 p.GetId(), p.GetAge(),-1, -1, -1,-1, 0, -1);
                                }
                        }
                        // random draw to continue in this pool or to sample a new one
                        if (uniform01Generator() < (1 - immunityLinkProbability)) {
                                break;
                        }
                }
        }

//        vector<double> populationBrackets2(100, 0.0);
//               for (auto& c : pools) {
//                               for (const auto& p : c.GetPool()) {
//                                       if (p->GetHealth().IsSusceptible()) {
//                                               populationBrackets2[p->GetAge()]++;
//                                       }
//                               }
//                       }
//               for (unsigned int age = 0; age < 30; age++) {
//                             std::cout << age << " -- " << populationBrackets2[age] << " -- " << immunityDistribution[age] << std::endl;
//
//                               }
}

//void Immunizer::Random_tmp(const SegmentedVector<ContactPool>& pools, vector<double>& immunityDistribution,
//                       double immunityLinkProbability,std::shared_ptr<Population> /*pop*/, const bool /*log_immunity*/)
//{
//        // Initialize a vector to count the population per age class [0-100].
//        vector<double> populationBrackets(100, 0.0);
//
//        // Count individuals per age class and set all "susceptible" individuals "immune".
//        // note: focusing on measles, we expect the number of susceptible individuals
//        // to be less compared to the number of immune.
//        // TODO but this is a generic simulator
//        for (auto& c : pools) {
//                for (const auto& p : c.GetPool()) {
//                        if (p->GetHealth().IsSusceptible()) {
//                                p->GetHealth().SetImmune();
//                                populationBrackets[p->GetAge()]++;
//                        }
//                }
//        }
//
//        // Sampler for int in [0, pools.size()) and for double in [0.0, 1.0).
//        const auto poolsSize          = static_cast<int>(pools.size());
//        auto       intGenerator       = m_rn_man.GetUniformIntGenerator(0, poolsSize, 0U);
//        auto       uniform01Generator = m_rn_man.GetUniform01Generator(0U);
//
//        // Calculate the number of susceptible individuals per age class.
//        unsigned int numSusceptible = 0;
//        for (unsigned int age = 0; age < 100; age++) {
//                auto temp_pop_num = populationBrackets[age];
//        		populationBrackets[age] = floor(populationBrackets[age] * (1 - immunityDistribution[age]));
//                numSusceptible += static_cast<unsigned int>(populationBrackets[age]);
//                std::cout << age << " -- " << populationBrackets[age] << " -- " << temp_pop_num << " -- " << immunityDistribution[age] << std::endl;
//        }
//
//        // Sample susceptible individuals, until all age-dependent quota are reached.
//        while (numSusceptible > 0) {
//                // random pool, random order of members
//                const ContactPool&   p_pool = pools[intGenerator()];
//                const auto           size   = static_cast<unsigned int>(p_pool.GetPool().size());
//                vector<unsigned int> indices(size);
//                iota(indices.begin(), indices.end(), 0U);
//                m_rn_man.Shuffle(indices, 0U);
//
//                // loop over members, in random order
//                for (unsigned int i_p = 0; i_p < size && numSusceptible > 0; i_p++) {
//                        Person& p = *p_pool.GetPool()[indices[i_p]];
//                        // if p is immune and his/her age class has not reached the quota => make susceptible
//                        if (p.GetHealth().IsImmune() && populationBrackets[p.GetAge()] > 0) {
//                                p.GetHealth().SetSusceptible();
//                                populationBrackets[p.GetAge()]--;
//                                numSusceptible--;
//                        }
//                        // random draw to continue in this pool or to sample a new one
//                        if (uniform01Generator() < (1 - immunityLinkProbability)) {
//                                break;
//                        }
//                }
//        }
//
//        vector<double> populationBrackets2(100, 0.0);
//        for (auto& c : pools) {
//                        for (const auto& p : c.GetPool()) {
//                                if (p->GetHealth().IsSusceptible()) {
//                                        populationBrackets2[p->GetAge()]++;
//                                }
//                        }
//                }
//        for (unsigned int age = 0; age < 30; age++) {
//                      std::cout << age << " -- " << populationBrackets2[age] << " -- " << immunityDistribution[age] << std::endl;
//
//                        }
//}

} // namespace stride
