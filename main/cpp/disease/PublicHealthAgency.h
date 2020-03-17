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
 * Header for the PublicHealthAgency class.
 */

#pragma once

#include "contact/ContactPool.h"
#include "util/RnMan.h"
#include "util/SegmentedVector.h"

#include <boost/property_tree/ptree_fwd.hpp>
#include <memory>

namespace stride {

class Population;
class Sim;

/**
 * Sets intervention measures.
 */
class PublicHealthAgency
{
public:
        /// Initializing PublicHealthAgency.
		PublicHealthAgency(const boost::property_tree::ptree& config, util::RnMan& rnMan);

        /// set telework features.
        void SetTelework(std::shared_ptr<Population> pop);

private:
        const boost::property_tree::ptree& m_config; ///< Run config.
        util::RnMan&                       m_rn_man; ///< Random number manager
};

} // namespace stride
