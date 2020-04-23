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
 * Interface for DaysOff classes.
 */

#pragma once

namespace stride {

/**
 * Interface definition.
 */
class DaysOffInterface
{
public:
        /// Whether today is a regular weekday (= NO weekend or holiday).
        virtual bool IsRegularWeekday() = 0;

        /// Whether today pre-schools are off.
        virtual bool IsPreSchoolOff() = 0;

        /// Whether today primary schools are off.
        virtual bool IsPrimarySchoolOff() = 0;

        /// Whether today secondary schools are off.
        virtual bool IsSecondarySchoolOff() = 0;

        /// Whether today college is off.
        virtual bool IsCollegeOff() = 0;

        /// Whether distancing measures are in place for work places
        virtual bool IsWorkplaceDistancingEnforced() = 0;

        /// Whether quarantine measures are in place for community contacts
        virtual bool IsCommunityDistancingEnforced() = 0;

         /// Whether contact tracing is in place
         virtual bool IsContactTracingActivated() = 0;

        /// Virtual destructor.
        virtual ~DaysOffInterface() = default;


};

} // namespace stride
