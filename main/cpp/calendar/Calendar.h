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
 * Header file for the Calendar class.
 */

#pragma once

#include <boost/property_tree/ptree_fwd.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

#include <algorithm>
#include <cstdlib>
#include <memory>
#include <vector>



namespace stride {

/**
 * Class that keeps track of the 'state' of simulated world.
 * E.g. what day it is, holidays, quarantines, ...
 */
class Calendar
{
public:
        /// Constructor
        explicit Calendar(const boost::property_tree::ptree& configPt,unsigned int num_days);

        /// Advance the simulated calendar by one day.
        void AdvanceDay();

        /// Current day of the month in the simulated calendar.
        std::size_t GetDay() const;

        /// Current day of the week (0 (Sunday), ..., 6 (Saturday)) in the simulated calendar.
        std::size_t GetDayOfTheWeek() const;

        /// Current month in the simulated calendar.
        std::size_t GetMonth() const;

        /// Current simulated day since the start of the simulation.
        unsigned short int GetSimulationDay() const;

        /// Current year in the simulated calendar.
        std::size_t GetYear() const;


        /// Check if today is a regular weekday (= NO weekend or holiday).
		bool IsRegularWeekday() const
		{
			return !(IsWeekend() || IsPublicHoliday());
		}

		/// Check if today pre-schools are off.
		bool IsPreSchoolOff() const
		{
			return IsWeekend() || IsPublicHoliday() || IsPreSchoolClosed();
		}

		/// Check if today primary schools are off.
		bool IsPrimarySchoolOff() const
		{
			return IsWeekend() || IsPublicHoliday() || IsPrimarySchoolClosed();
		}

		 /// Check if today secondary schools are off.
		bool IsSecondarySchoolOff() const
		{
			return IsWeekend() || IsPublicHoliday() || IsSecondarySchoolClosed();
		}

		/// Check if today college is off.
		bool IsCollegeOff() const
		{
			return IsWeekend() || IsPublicHoliday() || IsCollegeClosed();
		}

        /// Check if quarantine measures are in place
        bool IsWorkplaceDistancingEnforced() const
        {
			 return (std::find(m_workplace_distancing.begin(), m_workplace_distancing.end(), m_date) !=
					 m_workplace_distancing.end());
		}

        /// Check if quarantine measures are in place
		bool IsCommunityDistancingEnforced() const
		{
			 return (std::find(m_community_distancing.begin(), m_community_distancing.end(), m_date) !=
					 m_community_distancing.end());
		}

		/// Check if contact tracing is place
		bool IsContactTracingActivated() const
		{
			 return (std::find(m_contact_tracing.begin(), m_contact_tracing.end(), m_date) !=
					 m_contact_tracing.end());
		}

  		/// Check if universal testing is place
		bool IsUniversalTestingActivated() const
		{
			 return (std::find(m_universal_testing.begin(), m_universal_testing.end(), m_date) !=
					 m_universal_testing.end());
		}

		/// Check if household clustering is allowed
		bool IsHouseholdClusteringAllowed() const
		{
			 return (std::find(m_household_clustering.begin(), m_household_clustering.end(), m_date) !=
					 m_household_clustering.end());
		}

		unsigned int GetNumberOfImportedCases() const{

			return m_imported_cases[GetDayIndex(m_date)];
		}

private:

		unsigned short int GetDayIndex(boost::gregorian::date date) const;

		bool IsDatePartOfSimulation(boost::gregorian::date date) const
		{
			return m_date_start <= date && date < m_date_end;
		}

		/// Check if it's a public holiday.
		bool IsPublicHoliday() const
		{
			//return (std::find(m_public_holidays.begin(), m_public_holidays.end(), m_date) != m_public_holidays.end());
			return m_public_holidays_bool[GetDayIndex(m_date)];
		}

		/// Check if pre-schools are closed.
		bool IsPreSchoolClosed() const
		{
			 return (std::find(m_preschool_holidays.begin(), m_preschool_holidays.end(), m_date) !=
					 m_preschool_holidays.end());
		}

		/// Check if primary schools are closed.
		bool IsPrimarySchoolClosed() const
		{
			 return (std::find(m_primary_school_holidays.begin(), m_primary_school_holidays.end(), m_date) !=
					 m_primary_school_holidays.end());
		}

		/// Check if secondary schools are closed.
		bool IsSecondarySchoolClosed() const
		{
			 return (std::find(m_secondary_school_holidays.begin(), m_secondary_school_holidays.end(), m_date) !=
					 m_secondary_school_holidays.end());
		}

		/// Check if Colleges are closed.
		bool IsCollegeClosed() const
		{
			 return (std::find(m_college_holidays.begin(), m_college_holidays.end(), m_date) !=
					 m_college_holidays.end());
		}

		/// Check if it's weekend.
		bool IsWeekend() const
		{
			return (GetDayOfTheWeek() == 6 || GetDayOfTheWeek() == 0);
		}

		/// Initialize the calendar
        void Initialize(const boost::property_tree::ptree& configPt);

        boost::gregorian::date              m_date;                       ///< Current simulated date.
        boost::gregorian::date              m_date_start;                 ///< Start simulation.
        boost::gregorian::date              m_date_end;                   ///< End simulation.
        std::vector<boost::gregorian::date> m_public_holidays;            ///< Vector of public holidays
        std::vector<boost::gregorian::date> m_preschool_holidays;         ///< Vector of pre-school closure
        std::vector<boost::gregorian::date> m_primary_school_holidays;    ///< Vector of primary school closure
        std::vector<boost::gregorian::date> m_secondary_school_holidays;  ///< Vector of secondary school closure
        std::vector<boost::gregorian::date> m_college_holidays;           ///< Vector of college closure
        std::vector<boost::gregorian::date> m_workplace_distancing;       ///< Vector of days with social distancing enforcement for work places
        std::vector<boost::gregorian::date> m_community_distancing;       ///< Vector of days with social distancing enforcement in the community
        std::vector<boost::gregorian::date> m_contact_tracing;            ///< Vector of days with case finding measures
        std::vector<boost::gregorian::date> m_universal_testing;          ///< Vector of days with universal testing measures
        std::vector<boost::gregorian::date> m_household_clustering;       ///< Vector of days when household clusters are allowed

        std::vector<bool>m_public_holidays_bool;
        std::vector<unsigned int>m_imported_cases; ///<Vector of days when cases are imported (~daily seeding activated)

};

} // namespace stride
