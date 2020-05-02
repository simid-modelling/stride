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
		// Age does not determine non-compliance: set probability of being a non-complier to 1 for all ages
		for (unsigned int index_age = 0; index_age <= maxAge; index_age++) {
			nonComplianceDistribution.push_back(1);
		}
	}

	// RNG
	auto generatorInt = m_rn_man.GetUniformIntGenerator(0, static_cast<int>(popCount), 0U);
	auto generator0to1 = m_rn_man.GetUniform01Generator(0U);

	// Non compliance by district OR by pool OR random
	// Get type of non-compliance seeding (by district or random)
	// If no parameter is present in the config file, assume NO non-compliance
	boost::optional<string> nonComplianceType = m_config.get_optional<string>("run.non_compliance_type");
	if (nonComplianceType) {
		if (*nonComplianceType == "Random") {
			unsigned int targetNumNonCompliers = m_config.get<int>("run.num_non_compliers");
			assert((popCount >= targetNumNonCompliers) && "NonComplianceSeeder> Pop count has to exceed number of non-compliers.");

			vector<Id> nonCompliantPoolTypes;
			for (auto pooltype : m_config.get_child("run.non_compliant_pooltypes")) {
				nonCompliantPoolTypes.push_back(ToId(pooltype.second.get_value<string>()));
			}

			auto numNonCompliers = 0U;

			while (numNonCompliers < targetNumNonCompliers) {
				Person& p = population[generatorInt()];

				if (generator0to1() < nonComplianceDistribution[p.GetAge()]) {
					// Set person to be non-complier
					bool registered = RegisterNonComplier(pop, p, nonCompliantPoolTypes);
					// Update number of remaining non-compliers to be found
					if (registered) {
						numNonCompliers++;
					}

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

				vector<Id> nonCompliantPoolTypes;
				nonCompliantPoolTypes.push_back(Id::PrimaryCommunity);
				nonCompliantPoolTypes.push_back(Id::SecondaryCommunity);
				while (numNonCompliers < targetNumNonCompliers) {
					// Choose a random household in the hotspot
					unsigned int hhID = householdsInHotspot[generatorHH()];
					const ContactPool&   p_pool = population.CRefPoolSys().CRefPools(Id::Household)[hhID];
					// Choose a random person in this household
					auto generatorP = m_rn_man.GetUniformIntGenerator(0, static_cast<int>(p_pool.size()), 0U);
					Person& p = *p_pool[generatorP()];
					// Apply age-dependent probability of being a non-complier
					if (generator0to1() < nonComplianceDistribution[p.GetAge()]) {
						// Set person to be non-complier
						bool registered = RegisterNonComplier(pop, p, nonCompliantPoolTypes);
						// Update number of remaining non-compliers to be found
						if (registered) {
							numNonCompliers++;
						}
					}
				}
			}

		} else if (*nonComplianceType == "Pools") {
			const auto poolsFile = m_config.get<string>("run.non_compliance_pools_file");
			const ptree& pools_pt = FileSys::ReadPtreeFile(poolsFile);

			// Get type of pools
			const Id poolType = ToId(pools_pt.get<string>("non_compliant_pools.pooltype"));
			vector<Id> poolTypes;
			poolTypes.push_back(poolType);


			// Get ID of pools in which non-compliance occurs
			vector<int> nonCompliantPools;
			for (auto pool: pools_pt.get_child("non_compliant_pools.pools")) {
				int poolID = pool.second.get_value<int>();
				nonCompliantPools.push_back(poolID);
			}

			// Set individuals belonging to pools to non-compliant
			for (auto p : population.CRefPoolSys().CRefPools(poolType)) {
				const auto poolId = p.GetId();
				// Check if pool is one of non-compliant pools
				if (find(nonCompliantPools.begin(), nonCompliantPools.end(), poolId) != nonCompliantPools.end()) {
					// Make members non compliers in this pool
					for (unsigned int p_i = 0; p_i < p.size(); p_i++) {
						Person& person = *p[p_i];
						RegisterNonComplier(pop, person, poolTypes);
					}
				}
			}

		}
	}

	return pop;
}

bool NonComplianceSeeder::RegisterNonComplier(std::shared_ptr<Population> pop, Person& p, vector<Id> pooltypes)
{
	Population& population  = *pop;
	auto&       logger      = population.RefContactLogger();
	// Check if person is already a non-complier
	// And if they actually belong to one of the pools they are set to be a non-complier in
	bool belongsToPools = false;
	for (auto pooltype : pooltypes) {
		if (p.IsNonComplier(pooltype)) {
			return false;
		}
		if (p.GetPoolId(pooltype) != 0) {
			belongsToPools = true;
		}
	}
	if (not belongsToPools) {
		return false;
	}


	// Set person to be non-complier for the correct pooltypes
	for (auto type : pooltypes) {
		p.SetNonComplier(type);
	}
	// Log person details
	logger->info("[NCOM] {} {} {} {} {} {} {} {} {} {} {} {} {} {}", p.GetId(), p.GetAge(),
									p.GetPoolId(Id::Household), p.IsNonComplier(Id::Household),
									p.GetPoolId(Id::K12School), p.IsNonComplier(Id::K12School),
									p.GetPoolId(Id::College), p.IsNonComplier(Id::College),
									p.GetPoolId(Id::Workplace), p.IsNonComplier(Id::Workplace),
									p.GetPoolId(Id::PrimaryCommunity), p.IsNonComplier(Id::PrimaryCommunity),
									p.GetPoolId(Id::SecondaryCommunity), p.IsNonComplier(Id::SecondaryCommunity));
	return true;

}

} // namespace stride
