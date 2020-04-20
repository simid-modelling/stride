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

	// Get type of non-compliance seeding (by household or at random)
	// if no parameter is present in config file, assume NO non-compliance
	boost::optional<string> nonComplianceType = m_config.get_optional<string>("run.non_compliance_type");
	if (nonComplianceType) {
		if (*nonComplianceType == "Random") {
			unsigned int targetNumNonCompliers = m_config.get<int>("run.num_non_compliant_individuals");
			assert((popCount >= targetNumNonCompliers) && "NonComplianceSeeder> Pop count has to exceed number of surveyed.");
			 auto numNonCompliers = 0U;
			 auto generator = m_rn_man.GetUniformIntGenerator(0, static_cast<int>(popCount), 0U);
			 while (numNonCompliers < targetNumNonCompliers) {
				 Person& p = population[generator()];
				 if (p.IsNonComplier()) {
					 continue;
				 }

				 // Set person to be non-complier
				 RegisterNonComplier(pop, p);

				 // Update number of remaining samples
				 numNonCompliers++;
			 }

		} else if (*nonComplianceType == "Hotspots") {
			// Get IDs of households that are in non-compliance hotspots
			const auto hotspotsFile = m_config.get<string>("run.pools_in_hotspots_file");
			const ptree& hotspots_pt  = FileSys::ReadPtreeFile(hotspotsFile);
			vector<unsigned int> households_in_hotspots = vector<unsigned int>();
			for (auto child: hotspots_pt.get_child("hotspots")) {
				unsigned int hhID = child.second.get_value<int>();
				households_in_hotspots.push_back(hhID);
			}

			// Iterate over all persons and set to 'non-compliant' if household is in hotspot
			for (Person& p : population) {
				auto hhID = p.GetPoolId(Id::Household);
				if (find(households_in_hotspots.begin(), households_in_hotspots.end(), hhID) != households_in_hotspots.end()) {
					RegisterNonComplier(pop, p);
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
	p.SetNonComplier();
	// Log person details
	logger->info("[NCOM] {} {} {}", p.GetId(), p.GetAge(), p.GetPoolId(Id::Household));
}

} // namespace stride
