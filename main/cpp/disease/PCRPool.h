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
 * Header for the PCRPool class.
 */

#include <vector>

namespace stride {

class Person;

/// A PCR pool consists out of a number of individuals that is to be tested, simultaneously, in one PCR test, i.e., by pooling the samples.
class PCRPool 
{
public:
        PCRPool():m_individuals() {}
        
        void AddIndividual(Person* p) { return m_individuals.push_back(p); }

        const std::vector<Person*>& GetIndividuals() const { return m_individuals; } 

        bool operator< (const PCRPool& p) const {
            return m_individuals < p.m_individuals;
        }
private:
        std::vector<Person*> m_individuals;
};

} // namespace stride
