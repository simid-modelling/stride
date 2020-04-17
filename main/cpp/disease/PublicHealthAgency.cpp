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
 * Implementation for the PublicHealthAgency class.
 */

#include "PublicHealthAgency.h"

#include "pop/Population.h"
#include "util/FileSys.h"
#include "util/LogUtils.h"
#include "util/StringUtils.h"

#include <boost/property_tree/ptree.hpp>

namespace stride {

using namespace boost::property_tree;
using namespace stride::ContactType;
using namespace stride::util;
using namespace std;

void PublicHealthAgency::Initialize(const ptree& config){
	m_telework_probability   = config.get<double>("run.telework_probability",0);
	m_detection_probability  = config.get<double>("run.detection_probability",0);
	m_case_finding_efficency = config.get<double>("run.case_finding_efficency",0);
}

void PublicHealthAgency::SetTelework(std::shared_ptr<Population> pop, util::RnMan& rnMan)
{
	auto       generator   = rnMan.GetUniform01Generator();
	for (auto& p : *pop) {
		if(p.GetPoolId(ContactType::Id::Workplace) != 0 &&  generator() < m_telework_probability){
			p.SetTeleworking();
		}
	}
}

void PublicHealthAgency::PerformContactTracing(std::shared_ptr<Population> pop, util::RnMan& rnMan,
                                            unsigned short int simDay)
{

	cout << m_detection_probability << " -- "<< m_case_finding_efficency << endl;
	// perform case finding, only if the probability is > 0.0
	if (m_detection_probability <= 0.0) {
			return;
	}

	using namespace ContactType;
	auto  uniform01Gen = rnMan.GetUniform01Generator(0U);
	auto& logger       = pop->RefContactLogger();

	/// To allow iteration over pool types for the PublicHealthAgency.
	std::initializer_list<Id> AgencyPoolIdList{Id::Household,Id::Workplace, Id::K12School, Id::College};

	for (auto& p_case : *pop) {
			if (p_case.GetHealth().IsSymptomatic() && uniform01Gen() < m_detection_probability) {

				// set case in quarantine
				p_case.GetHealth().StartQuarantine();

				// loop over his/her contacts
				for (Id typ : AgencyPoolIdList) {


					const auto& pools  = pop->CRefPoolSys().CRefPools(typ);
					const auto  poolId = p_case.GetPoolId(typ);
					if (poolId == 0) {
							continue;
					}

					for (const auto& p_member : pools[poolId].GetPool()) {
							if (p_case != *p_member && p_member->GetHealth().IsInfected() &&
								uniform01Gen() < m_case_finding_efficency) {

									// start quarantine measure
									p_member->GetHealth().StartQuarantine();

									// TODO: check log_level
									logger->info("[QUAR] {} {} {} {} {} {} {} {}",
												 p_member->GetId(), p_member->GetAge(),
												 ToString(typ), poolId, p_case.GetId(),
												 p_case.GetAge(), simDay, p_case.GetHealth().GetIdIndexCase());
					}
				}
			}
		}
	}
}


} // namespace stride
