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
 *  Copyright 2020, Willem L, Kuylen E, Broeckhove J, Libin P
 */

/**
 * @file
 * Implementation file for the Calendar class.
 */

#include "Calendar.h"

#include "util/FileSys.h"

#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>

namespace stride {

using namespace std;
using namespace boost::property_tree::json_parser;
using namespace stride::util;
using boost::property_tree::ptree;


Calendar::Calendar(const ptree& configPt,unsigned int num_days) :
		m_date(), m_date_start(), m_date_end(), m_public_holidays(), m_preschool_holidays(), m_primary_school_holidays(),
		  m_secondary_school_holidays(), m_college_holidays(), m_workplace_distancing(),
		m_community_distancing(), m_contact_tracing(), m_universal_testing(), m_household_clustering(),
		m_public_holidays_bool(num_days), m_imported_cases(num_days,0U)
{
        // Set start date
        m_date = boost::gregorian::from_simple_string(configPt.get<string>("run.start_date", "2020-01-01"));
        m_date_start = m_date;
        m_date_end = m_date + boost::gregorian::days(num_days);

        //cout << "setup calendar :: " << m_public_holidays_bool.size() << endl;

        Initialize(configPt);
}

void Calendar::AdvanceDay()
{
        m_date = m_date + boost::gregorian::date_duration(1);
}

size_t Calendar::GetDay() const { return m_date.day(); }

size_t Calendar::GetDayOfTheWeek() const { return m_date.day_of_week(); }

size_t Calendar::GetMonth() const { return m_date.month(); }

unsigned short int Calendar::GetSimulationDay() const {

	return GetDayIndex(m_date);
}


unsigned short int Calendar::GetDayIndex(boost::gregorian::date date) const{

	if(date == m_date_start){ return 0; }

	return (date - m_date_start).days();
}


size_t Calendar::GetYear() const { return m_date.year(); }




void Calendar::Initialize(const ptree& configPt)
{
        // Load json file
        ptree holidaysPt;
        {
                const string        fName{configPt.get<string>("run.holidays_file", "holidays_flanders_2020.json")};
                const filesys::path fPath{FileSys::GetDataDir() /= fName};
                if (!is_regular_file(fPath)) {
                        throw runtime_error(string(__func__) + "Holidays file " + fPath.string() + " not present.");
                }
                read_json(fPath.string(), holidaysPt);
        }

        // Read in holidays
        for (int i = 1; i < 13; i++) {
                const auto month = to_string(i);
                const auto year  = holidaysPt.get<string>("year", "2020");
                const auto lead  = string(year).append("-").append(month).append("-");

                // read in general holidays
                for (const auto& date : holidaysPt.get_child("general." + month)) {
                        const auto d = string(lead).append(date.second.get_value<string>());
                        const auto d_date = boost::gregorian::from_simple_string(d);

                        if(IsDatePartOfSimulation(d_date)){
                        	m_public_holidays_bool[GetDayIndex(d_date)] = true;
                        }
                }

                // read in pre-school holidays
                for (const auto& date : holidaysPt.get_child("preschool." + month)) {
                        const string d = string(lead).append(date.second.get_value<string>());
                        m_preschool_holidays.push_back(boost::gregorian::from_simple_string(d));
                }
                // read in primary school holidays
				for (const auto& date : holidaysPt.get_child("primary_school." + month)) {
						const string d = string(lead).append(date.second.get_value<string>());
						m_primary_school_holidays.push_back(boost::gregorian::from_simple_string(d));
				}

                // read in secondary school holidays
				for (const auto& date : holidaysPt.get_child("secondary_school." + month)) {
						const string d = string(lead).append(date.second.get_value<string>());
						m_secondary_school_holidays.push_back(boost::gregorian::from_simple_string(d));
				}

				// read in college holidays
                for (const auto& date : holidaysPt.get_child("college." + month)) {
                        const string d = string(lead).append(date.second.get_value<string>());
                        m_college_holidays.push_back(boost::gregorian::from_simple_string(d));
                }
                // read in work place distancing data (if present)
                if(holidaysPt.count("workplace_distancing") != 0){
					for (const auto& date : holidaysPt.get_child("workplace_distancing." + month)) {
							const string d = string(lead).append(date.second.get_value<string>());
							m_workplace_distancing.push_back(boost::gregorian::from_simple_string(d));
					}
                }

                // read in community distancing data (if present)
				if(holidaysPt.count("community_distancing") != 0){
					for (const auto& date : holidaysPt.get_child("community_distancing." + month)) {
							const string d = string(lead).append(date.second.get_value<string>());
							m_community_distancing.push_back(boost::gregorian::from_simple_string(d));
					}
				}

				// read in contact tracing data (if present)
				if(holidaysPt.count("contact_tracing") != 0){
					for (const auto& date : holidaysPt.get_child("contact_tracing." + month)) {
							const string d = string(lead).append(date.second.get_value<string>());
							m_contact_tracing.push_back(boost::gregorian::from_simple_string(d));
					}
				}

				// read in universal testing data (if present)
				if(holidaysPt.count("universal_testing") != 0){
					for (const auto& date : holidaysPt.get_child("universal_testing." + month)) {
							const string d = string(lead).append(date.second.get_value<string>());
							m_universal_testing.push_back(boost::gregorian::from_simple_string(d));
					}
				}

				// read in household clustering data (if present)
				if(holidaysPt.count("household_clustering") != 0){
					for (const auto& date : holidaysPt.get_child("household_clustering." + month)) {
							const string d = string(lead).append(date.second.get_value<string>());
							m_household_clustering.push_back(boost::gregorian::from_simple_string(d));
					}
				}

				// read in imported cases
				if(holidaysPt.count("import_cases") != 0){
					for (const auto& date : holidaysPt.get_child("import_cases." + month)) {
						const auto d = string(lead).append(date.second.get_value<string>());
						const auto d_date = boost::gregorian::from_simple_string(d);

						unsigned int num_cases = configPt.get<unsigned int>("run.num_daily_imported_cases",0);
						//unsigned int num_cases = field_num_cases.second.get_value<unsigned int>();
//						cout << num_cases << " * " << d_date << " " << IsDatePartOfSimulation(d_date) << endl;
//						cout <<  (m_date_start <= d_date) << " & " << (d_date < m_date_end) << endl;
						if(IsDatePartOfSimulation(d_date)){
							m_imported_cases[GetDayIndex(d_date)] = num_cases;
						}
					}
				} else { // if no calendar info present, use the same value throuthout the simulation
					unsigned int num_cases = configPt.get<unsigned int>("run.num_daily_imported_cases",0);
					for (unsigned int day_index = 0 ; day_index < m_imported_cases.size() ; day_index++){
						//cout << m_imported_cases.size() << " * " << day_index << endl;
						m_imported_cases[day_index] = num_cases;
					}
				}
        }
}



} // namespace stride
