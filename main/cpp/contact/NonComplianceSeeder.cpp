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
#include "util/RnMan.h"

#include <boost/property_tree/ptree.hpp>
#include <cassert>

using namespace boost::property_tree;
// using namespace stride::ContactType;
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
				 p.SetNonComplier();

				 // Update number of remaining samples
				 numNonCompliers++;
			 }

		} else if (*nonComplianceType == "Hotspots") {

		}
	}

	return pop;
}

} // namespace stride

/*


namespace stride {
{
                while (numSamples < numSurveyed) {
                        Person& p = population[generator()];
                        if (p.IsSurveyParticipant()) {
                                continue;
                        }

                        // register new participant
                        RegisterParticipant(pop,p);

                        // update number of remaining samples
                        numSamples++;
                }
        return pop;
}

void SurveySeeder::RegisterParticipant(std::shared_ptr<Population> pop, Person& p)
{

	const string logLevel = m_config.get<string>("run.contact_log_level", "None");
	if (logLevel != "None") {
		 Population& population  = *pop;

		auto&       poolSys     = population.CRefPoolSys();
		auto&       logger      = population.RefContactLogger();

		// set person flag to be survey participant
		p.ParticipateInSurvey();

		// log person details
		const auto h    = p.GetHealth();
		const auto pHH  = p.GetPoolId(Id::Household);
		const auto pK12 = p.GetPoolId(Id::K12School);
		const auto pC   = p.GetPoolId(Id::College);
		const auto pW   = p.GetPoolId(Id::Workplace);
		const auto pPC  = p.GetPoolId(Id::PrimaryCommunity);
		const auto pSC  = p.GetPoolId(Id::SecondaryCommunity);
		// TODO: create a more elegant solution
		// - gengeopop population ==>> unique pool id over all pool types, so ID != index in
		// poolType-vector
		// - default population   ==>> unique pool id per pool type, so ID == index in
		// poolType-vector
		//if (p.GetPoolId(Id::SecondaryCommunity) < poolSys[Id::SecondaryCommunity].size()) {
		if (pSC < poolSys.CRefPools<Id::SecondaryCommunity>().size()) {
				logger->info("[PART] {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}", p.GetId(),
					 p.GetAge(), pHH, pK12, pC, pW, h.IsSusceptible(), h.IsInfected(), h.IsInfectious(),
					 h.IsRecovered(), h.IsImmune(), h.GetStartInfectiousness(), h.GetStartSymptomatic(),
					 h.GetEndInfectiousness(), h.GetEndSymptomatic(),
					 poolSys.CRefPools<Id::Household>()[pHH].GetPool().size(),
					 poolSys.CRefPools<Id::K12School>()[pK12].GetPool().size(),
					 poolSys.CRefPools<Id::College>()[pC].GetPool().size(),
					 poolSys.CRefPools<Id::Workplace>()[pW].GetPool().size(),
					 poolSys.CRefPools<Id::PrimaryCommunity>()[pPC].GetPool().size(),
					 poolSys.CRefPools<Id::SecondaryCommunity>()[pSC].GetPool().size(),
					 p.IsTeleworking());
		} else {
				logger->info("[PART] {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}", p.GetId(),
					 p.GetAge(), pHH, pK12, pC, pW, h.IsSusceptible(), h.IsInfected(), h.IsInfectious(),
					 h.IsRecovered(), h.IsImmune(), h.GetStartInfectiousness(), h.GetStartSymptomatic(),
					 h.GetEndInfectiousness(), h.GetEndSymptomatic(),
					 -1, -1, -1, -1, -1, -1 -1 );
		}
	 }
}


} // namespace stride
*/
