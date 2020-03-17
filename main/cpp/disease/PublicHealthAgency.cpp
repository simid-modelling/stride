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

#include "disease/Immunizer.h"
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

PublicHealthAgency::PublicHealthAgency(const ptree& config, RnMan& rnMan) : m_config(config), m_rn_man(rnMan) {}

void PublicHealthAgency::SetTelework(std::shared_ptr<Population> pop)
{


        // --------------------------------------------------------------
        // set telework
        // --------------------------------------------------------------
        const auto telework_probability = m_config.get<double>("run.telework_probability",0);
        auto       generator   = m_rn_man.GetUniform01Generator();

        for (auto& p : *pop) {
			if(p.GetPoolId(ContactType::Id::Workplace) != 0 &&  generator() < telework_probability){
				p.SetTeleworking();
			}
        }


}


} // namespace stride
