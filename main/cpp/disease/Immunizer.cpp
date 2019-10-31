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

		// retrieve the maximum age in the population
		unsigned int maxAge = pop->GetMaxAge();

		// Initialize a vector to count the population per age class [0-100].
        vector<double> populationBrackets(maxAge+1, 0.0);

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
        for (unsigned int age = 0; age <= maxAge; age++) {
                populationBrackets[age] = floor(populationBrackets[age] * immunityDistribution[age]);
                numImmune += static_cast<unsigned int>(populationBrackets[age]);

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
                                	logger->info("[VACC] {} {} {} {} {} {}",
                                				 p.GetId(), p.GetAge(),ToString(p_pool.GetType()), p_pool.GetId(), p_pool.HasInfant(),0);
                                }
                        }
                        // random draw to continue in this pool or to sample a new one
                        if (uniform01Generator() < (1 - immunityLinkProbability)) {
                                break;
                        }
                }
        }
}


} // namespace stride
