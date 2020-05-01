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
 *  Copyright 2020, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Implementation for the NonComplianceSeeder class.
 */

#include "NonComplianceSeeder.h"

#include "pop/Population.h"
#include "util/Exception.h"
#include "util/FileSys.h"
#include "util/RnMan.h"

#include <boost/property_tree/ptree.hpp>
#include <cassert>

using namespace boost::property_tree;
using namespace stride::ContactType;
using namespace stride::util;
using namespace std;

namespace stride {

NonComplianceSeeder::NonComplianceSeeder(const ptree& config, RnMan& rnMan) : m_config(config), m_rn_man(rnMan) {}

shared_ptr<Population> NonComplianceSeeder::Seed(shared_ptr<Population> pop)
{
	Population& population = *pop;
	const auto  popCount    = static_cast<unsigned int>(population.size() - 1);

	assert((popCount >= 1U) && "NonComplianceSeeder> Population count zero unacceptable.");

	// Non-compliance by age
	// retrieve the maximum age in the population
	unsigned int maxAge = pop->GetMaxAge();

	std::vector<double> nonComplianceDistribution;
	boost::optional<string> nonComplianceByAgeFile = m_config.get_optional<string>("run.non_compliance_by_age_file");
	if (nonComplianceByAgeFile) {
		const ptree& nonComplianceByAge_pt = FileSys::ReadPtreeFile(*nonComplianceByAgeFile);
		for (unsigned int index_age = 0; index_age <= maxAge; index_age++) {
			auto fractionNonCompliant = nonComplianceByAge_pt.get<double>("fraction_non_compliers.age" + std::to_string(index_age));
			nonComplianceDistribution.push_back(fractionNonCompliant);
		}
	} else {
		cout << "NO by age file" << endl;
		// Age does not determine non-compliance: set probability of being a non-complier to 1 for all ages
		for (unsigned int index_age = 0; index_age <= maxAge; index_age++) {
			nonComplianceDistribution.push_back(1);
		}
	}

	// RNG
	auto generatorInt = m_rn_man.GetUniformIntGenerator(0, static_cast<int>(popCount), 0U);
	auto generator0to1 = m_rn_man.GetUniform01Generator(0U);

	// Non compliance by district OR random
	// Get type of non-compliance seeding (by district or random)
	// If no parameter is present in the config file, assume NO non-compliance
	boost::optional<string> nonComplianceType = m_config.get_optional<string>("run.non_compliance_type");
	if (nonComplianceType) {
		if (*nonComplianceType == "Random") {
			unsigned int targetNumNonCompliers = m_config.get<int>("run.num_non_compliers");
			assert((popCount >= targetNumNonCompliers) && "NonComplianceSeeder> Pop count has to exceed number of non-compliers.");

			auto numNonCompliers = 0U;

			while (numNonCompliers < targetNumNonCompliers) {
				Person& p = population[generatorInt()];
				if (p.IsNonComplier(Id::PrimaryCommunity) or (p.IsNonComplier(Id::SecondaryCommunity))) {
					continue;
				}
				if (generator0to1() < nonComplianceDistribution[p.GetAge()]) {
					// Set person to be non-complier
					RegisterNonComplier(pop, p);
					// Update number of remaining non-compliers to be found
					numNonCompliers++;
				}
			}
		} else if (*nonComplianceType == "Hotspots") {
			// Calculate the number of non-compliers per district
			// = number of individuals in households in district * fraction non-compliers in district
			const auto hotspotsFile = m_config.get<string>("run.non_compliance_hotspots_file");
			const ptree& hotspots_pt = FileSys::ReadPtreeFile(hotspotsFile);
			for (auto district: hotspots_pt.get_child("hotspots")) {
				// Calculate total number of persons in hotspot
				unsigned int totalPopHotspot = 0U;
				vector<int> householdsInHotspot;
				for (auto household: district.second.get_child("households")) {
					auto hhID = household.second.get_value<int>();
					householdsInHotspot.push_back(hhID);
					auto hhSize = population.CRefPoolSys().CRefPools(Id::Household)[hhID].size();
					totalPopHotspot += hhSize;
				}
				// Calculate the number of non-compliers in hotspot
				double fractionNonCompliersInHotspot = district.second.get<double>("fraction_non_compliers");
				unsigned int targetNumNonCompliers = floor(totalPopHotspot * fractionNonCompliersInHotspot);

				// Sample non-compliers
				auto generatorHH = m_rn_man.GetUniformIntGenerator(0, static_cast<int>(householdsInHotspot.size()), 0U);
				unsigned int numNonCompliers = 0U;

				while (numNonCompliers < targetNumNonCompliers) {
					// Choose a random household in the hotspot
					unsigned int hhID = householdsInHotspot[generatorHH()];
					const ContactPool&   p_pool = population.CRefPoolSys().CRefPools(Id::Household)[hhID];
					// Choose a random person in this household
					auto generatorP = m_rn_man.GetUniformIntGenerator(0, static_cast<int>(p_pool.size()), 0U);
					Person& p = *p_pool[generatorP()];
					// Check that person is not already a non-complier
					if (p.IsNonComplier(Id::PrimaryCommunity) or p.IsNonComplier(Id::SecondaryCommunity)) {
						continue;
					}
					// Apply age-dependent probability of being a non-complier
					if (generator0to1() < nonComplianceDistribution[p.GetAge()]) {
						// Set person to be non-complier
						RegisterNonComplier(pop, p);
						// Update number of remaining non-compliers to be found
						numNonCompliers++;
					}
				}
			}

		}
	}

	return pop;
}

void NonComplianceSeeder::RegisterNonComplier(std::shared_ptr<Population> pop, Person& p)
{
	Population& population  = *pop;
	auto&       logger      = population.RefContactLogger();
	// Set person to be non-complier
	p.SetNonComplier(Id::PrimaryCommunity);
	p.SetNonComplier(Id::SecondaryCommunity);
	// Log person details
	logger->info("[NCOM] {} {} {}", p.GetId(), p.GetAge(), p.GetPoolId(Id::Household));
}

} // namespace stride
