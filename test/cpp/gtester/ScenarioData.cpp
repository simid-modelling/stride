
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
 *  Copyright 2018 Willem L, Kuylen E, Stijven S & Broeckhove J
 */

/**
 * @file
 * Implementation of scenario tests data.
 */

#include "ScenarioData.h"

#include "util/RunConfigManager.h"

#include <boost/property_tree/ptree.hpp>
#include <map>

using namespace std;
using namespace stride;
using namespace stride::util;
using boost::property_tree::ptree;

namespace Tests {

tuple<ptree, unsigned int, double> ScenarioData::Get(string tag)
{
        ptree pt = tag.substr(0, 2) != "r0" ? RunConfigManager::Create("TestsInfluenza")
                                            : RunConfigManager::Create("TestsMeasles");

        const map<string, unsigned int> targets_default = {
            {"influenza_a", 534000U}, {"influenza_b", 0U}, {"influenza_c", 5U}, {"measles_16", 240000U},
            {"measles_26", 600000U},  {"r0_0", 1200U},     {"r0_4", 5400U},     {"r0_8", 18800U},
            {"r0_12", 45000U},        {"r0_16", 75000U}};

        const map<string, double> margins_default = {
            {"influenza_a", 1.0e-02}, {"influenza_b", 0.0}, {"influenza_c", 2.0e-02}, {"measles_16", 1.0e-02},
            {"measles_26", 2.0e-02},  {"r0_0", 5.0e-02},    {"r0_4", 1.0e-01},        {"r0_8", 5.0e-02},
            {"r0_12", 5.0e-02},       {"r0_16", 5.0e-02}};

        unsigned int target;
        double       margin;
        target = targets_default.at(tag);
        margin = margins_default.at(tag);

        if (tag == "influenza_b") {
                pt.put("run.seeding_rate", 0.0);
        }
        if (tag == "influenza_c") {
                pt.put("run.seeding_rate", (1 - 0.9991) / 100);
                pt.put("run.immunity_rate", 0.9991);
        }
        if (tag == "measles_16") {
                pt.put("run.disease_config_file", "disease_measles.xml");
                pt.put("run.r0", 16U);
        }
        if (tag == "measles_26") {
                pt.put("run.disease_config_file", "disease_measles.xml");
                pt.put("run.r0", 26U);
                pt.put("run.num_days", 200U);
        }

        if (tag == "r0_0") {
                pt.put("run.r0", 0.0);
        }
        if (tag == "r0_4") {
                pt.put("run.r0", 4.0);
        }
        if (tag == "r0_8") {
                pt.put("run.r0", 8.0);
        }
        if (tag == "r0_12") {
                pt.put("run.r0", 12.0);
        }
        if (tag == "r0_16") {
                pt.put("run.r0", 16.0);
        }
        return make_tuple(pt, target, margin);
}

} // namespace Tests
