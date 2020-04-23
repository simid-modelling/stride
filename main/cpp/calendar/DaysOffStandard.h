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
 *  Copyright 2017, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * DaysOffStandard class.
 */

#pragma once

#include "calendar/Calendar.h"
#include "calendar/DaysOffInterface.h"

#include <memory>

namespace stride {

/**
 * Standard situation for days off from work and school.
 */
class DaysOffStandard : public DaysOffInterface
{
public:
        /// Initialize calendar.
        explicit DaysOffStandard(std::shared_ptr<Calendar> cal) : m_calendar(std::move(cal)) {}

        /// See DaysOffInterface.
        bool IsRegularWeekday() override { return !(m_calendar->IsWeekend() || m_calendar->IsPublicHoliday()); }

        /// See DaysOffInterface.
        bool IsPreSchoolOff() override
        {
        	return m_calendar->IsWeekend() || m_calendar->IsPublicHoliday() || m_calendar->IsPreSchoolClosed();
        }

        /// See DaysOffInterface.
		bool IsPrimarySchoolOff() override
		{
			return m_calendar->IsWeekend() || m_calendar->IsPublicHoliday() || m_calendar->IsPrimarySchoolClosed();
		}

		/// See DaysOffInterface.
		bool IsSecondarySchoolOff() override
		{
			return m_calendar->IsWeekend() || m_calendar->IsPublicHoliday() || m_calendar->IsSecondarySchoolClosed();
		}

        /// See DaysOffInterface.
		bool IsCollegeOff() override
		{
			return m_calendar->IsWeekend() || m_calendar->IsPublicHoliday() || m_calendar->IsCollegeClosed();
		}

        /// See DaysOffInterface.
        bool IsWorkplaceDistancingEnforced() override
        {
        	return m_calendar->IsWorkplaceDistancingMandated();
        }

		/// See DaysOffInterface.
		bool IsCommunityDistancingEnforced() override
		{
			return m_calendar->IsCommunityDistancingMandated();
		}

		/// See DaysOffInterface.
		bool IsContactTracingActivated() override
		{
			return m_calendar->IsContactTracingActivated();
		}


private:
        std::shared_ptr<Calendar> m_calendar; ///< Management of calendar.
};

} // namespace stride
