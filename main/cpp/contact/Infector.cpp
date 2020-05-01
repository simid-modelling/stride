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
 *  Copyright 2020, Willem L, Kuylen E, Broeckhove J
 */

/**
 * @file
 * Implementation of Infector algorithms.
 */

#include "Infector.h"

#include "ContactPool.h"
#include "calendar/Calendar.h"
#include "pop/Person.h"

using namespace std;

namespace {

/// Primary LOG_POLICY policy, implements LogMode::None.
/// \tparam LL
template <ContactLogMode::Id LL>
class LOG_POLICY
{
public:
        static void Contact(const std::shared_ptr<spdlog::logger>&, const Person*, const Person*, ContactType::Id,
                            unsigned short int, const double, const double)
        {
        }

        static void Trans(const std::shared_ptr<spdlog::logger>&, const Person*, const Person*, ContactType::Id,
                          unsigned short int, unsigned int)
        {
        }
};

/// Specialized LOG_POLICY policy LogMode::Transmissions.
template <>
class LOG_POLICY<ContactLogMode::Id::Transmissions>
{
public:
        static void Contact(const std::shared_ptr<spdlog::logger>&, const Person*, const Person*, ContactType::Id,
                            unsigned short int, const double, const double)
        {
        }

        static void Trans(const std::shared_ptr<spdlog::logger>& logger, const Person* p1, const Person* p2,
                          ContactType::Id type, unsigned short int sim_day, unsigned int id_index_case)
        {
                logger->info("[TRAN] {} {} {} {} {} {} {} {} {} {} {} {}", p2->GetId(), p1->GetId(), p2->GetAge(), p1->GetAge(),
                             ToString(type), sim_day, id_index_case,
							 p2->GetHealth().GetStartInfectiousness(),p2->GetHealth().GetEndInfectiousness(),
							 p2->GetHealth().GetStartSymptomatic(),p2->GetHealth().GetEndSymptomatic(),
							 p1->GetHealth().IsSymptomatic());
        }
};

/// Specialized LOG_POLICY policy LogMode::All.
template <>
class LOG_POLICY<ContactLogMode::Id::All>
{
public:
        static void Contact(const std::shared_ptr<spdlog::logger>& logger, const Person* p1, const Person* p2,
                            ContactType::Id type, unsigned short int sim_day, const double cProb, const double tProb)
        {
                if (p1->IsSurveyParticipant()) {
                        logger->info("[CONT] {} {} {} {} {} {} {} {} {} {} {} {} {} {}", p1->GetId(), p1->GetAge(),
                                     p2->GetAge(), static_cast<unsigned int>(type == ContactType::Id::Household),
                                     static_cast<unsigned int>(type == ContactType::Id::K12School),
                                     static_cast<unsigned int>(type == ContactType::Id::College),
                                     static_cast<unsigned int>(type == ContactType::Id::Workplace),
                                     static_cast<unsigned int>(type == ContactType::Id::PrimaryCommunity),
                                     static_cast<unsigned int>(type == ContactType::Id::SecondaryCommunity), sim_day,
									 cProb, tProb,p2->GetHealth().IsSymptomatic(),p1->GetHealth().IsSymptomatic());
                }
        }

        static void Trans(const std::shared_ptr<spdlog::logger>& logger, const Person* p1, const Person* p2,
                          ContactType::Id type, unsigned short int sim_day, unsigned int id_index_case)
        {
                logger->info("[TRAN] {} {} {} {} {} {} {} {} {} {} {} {}", p2->GetId(), p1->GetId(), p2->GetAge(), p1->GetAge(),
                             ToString(type), sim_day, id_index_case,
							 p2->GetHealth().GetStartInfectiousness(),p2->GetHealth().GetEndInfectiousness(),
							 p2->GetHealth().GetStartSymptomatic(),p2->GetHealth().GetEndSymptomatic(),
							 p1->GetHealth().IsSymptomatic());
        }
};

} // namespace

namespace {

using namespace stride;
using namespace stride::ContactType;

inline double GetContactProbability(const AgeContactProfile& profile, const Person* p1,const Person* p2,
		size_t pool_size, const ContactType::Id pType, double cnt_reduction_work, double cnt_reduction_other,
		double cnt_reduction_school, double cnt_reduction_intergeneration, unsigned int cnt_reduction_intergeneration_cutoff)
{
        // get the reference number of contacts, given age and age-contact profile
		const double reference_num_contacts_p1{profile[EffectiveAge(static_cast<unsigned int>(p1->GetAge()))]};
        const double reference_num_contacts_p2{profile[EffectiveAge(static_cast<unsigned int>(p2->GetAge()))]};
        const double potential_num_contacts{static_cast<double>(pool_size - 1)};

        // calculate the contact probability based on the (possible) number of contacts
        double individual_contact_probability_p1 = reference_num_contacts_p1 / potential_num_contacts;
        double individual_contact_probability_p2 = reference_num_contacts_p2 / potential_num_contacts;

        // use the minimum of both age-specific probabilities
        double contact_probability = individual_contact_probability_p1 ;
		if(individual_contact_probability_p2 < individual_contact_probability_p1){
			contact_probability = individual_contact_probability_p2;
		}

		// assume fully connected households
	    if(pType == Id::Household){
	    	contact_probability = 0.999;
	    }

        // limit probability to 0.999
        if (contact_probability >= 1) {
        	contact_probability = 0.999;
        }

        // account for social distancing at work and in the community
        if(pType == Id::Workplace){
        	contact_probability = contact_probability * (1-cnt_reduction_work);
        }

		// persons who are non-compliers do not apply contact reduction in community
        if((pType == Id::PrimaryCommunity || pType == Id::SecondaryCommunity) and not (p1->IsNonComplier(pType) and not (p2->IsNonComplier(pType)))){

			// apply intergeneration distancing factor if age cutoff is > 0 and at least one age is > cutoff
			if((cnt_reduction_intergeneration > 0) &&
				((p1->GetAge() > cnt_reduction_intergeneration_cutoff) || (p2->GetAge() > cnt_reduction_intergeneration_cutoff))){
				contact_probability = contact_probability * (1-cnt_reduction_intergeneration);
			} else {
				// apply uniform community distancing
				contact_probability = contact_probability * (1-cnt_reduction_other);
			}

		}
        // account for social distancing at work and in the community
		if(pType == Id::K12School){
			contact_probability = contact_probability * (1-cnt_reduction_school);
        }


        return contact_probability;
}

} // namespace

namespace stride {

//-------------------------------------------------------------------------------------------------
// Definition for ContactLogMode::Contacts,
// both with track_index_case false and true.
//-------------------------------------------------------------------------------------------------
template <ContactLogMode::Id LL, bool TIC, bool TO>
void Infector<LL, TIC, TO>::Exec(ContactPool& pool, const AgeContactProfile& profile,
                                 const TransmissionProfile& transProfile, ContactHandler& cHandler,
                                 unsigned short int simDay, shared_ptr<spdlog::logger> cLogger,
								 double cnt_reduction_work, double cnt_reduction_other, double cnt_reduction_school,
								 double cnt_reduction_intergeneration, unsigned int cnt_reduction_intergeneration_cutoff)
{
        using LP = LOG_POLICY<LL>;

        // set up some stuff
        const auto  pType    = pool.m_pool_type;
        const auto& pMembers = pool.m_members;
        const auto  pSize    = pMembers.size();
        const auto  tProb    = transProfile.GetProbability();

        // check all contacts
        for (size_t i_person1 = 0; i_person1 < pSize; i_person1++) {
                // check if member is present today
                const auto p1 = pMembers[i_person1];
                if (!p1->IsInPool(pType)) {
                        continue;
                }
                // loop over possible contacts (contacts can be initiated by each member)
                for (size_t i_person2 = i_person1 + 1; i_person2 < pSize; i_person2++) {
                        // check if not the same person
                        if (i_person1 == i_person2) {
                                continue;
                        }
                        // check if member is present today
                        const auto p2 = pMembers[i_person2];
                        if (!p2->IsInPool(pType)) {
                                continue;
                        }
                        // check for contact
                        const double cProb = GetContactProbability(profile, p1, p2, pSize, pType,
                        		cnt_reduction_work, cnt_reduction_other,cnt_reduction_school,cnt_reduction_intergeneration,cnt_reduction_intergeneration_cutoff);
                        if (cHandler.HasContact(cProb)) {
                                // log contact if person 1 is participating in survey
                                LP::Contact(cLogger, p1, p2, pType, simDay, cProb, tProb * p1->GetHealth().GetRelativeTransmission(p2->GetAge()));
                                // log contact if person 2 is participating in survey
                                LP::Contact(cLogger, p2, p1, pType, simDay, cProb, tProb * p2->GetHealth().GetRelativeTransmission(p1->GetAge()));

                                // transmission & infection.
                                if (cHandler.HasTransmission(tProb * p1->GetHealth().GetRelativeTransmission(p2->GetAge()))) {
                                        auto& h1 = p1->GetHealth();
                                        auto& h2 = p2->GetHealth();
                                        // No secondary infections with TIC; just mark p2 'recovered'
                                        if (h1.IsInfectious() && h2.IsSusceptible()) {
                                                h2.StartInfection(h1.GetIdIndexCase(),p1->GetId());
                                                if (TIC)
                                                        h2.StopInfection();
                                                LP::Trans(cLogger, p1, p2, pType, simDay, h1.GetIdIndexCase());
                                        } else if (h2.IsInfectious() && h1.IsSusceptible()) {
                                                h1.StartInfection(h2.GetIdIndexCase(),p2->GetId());
                                                if (TIC)
                                                        h1.StopInfection();
                                                LP::Trans(cLogger, p2, p1, pType, simDay, h2.GetIdIndexCase());
                                        }
                                }
                        }
                }
        }
}

//-------------------------------------------------------------------------------------------
// Definition for ContactLogMode::None and ContactLogMode::Transmission
// both with track_index_case false and true.
//-------------------------------------------------------------------------------------------
template <ContactLogMode::Id LL, bool TIC>
void Infector<LL, TIC, true>::Exec(ContactPool& pool, const AgeContactProfile& profile,
                                   const TransmissionProfile& transProfile, ContactHandler& cHandler,
                                   unsigned short int simDay, shared_ptr<spdlog::logger> cLogger,
								   double cnt_reduction_work, double cnt_reduction_other, double cnt_reduction_school,
								   double cnt_reduction_intergeneration, unsigned int cnt_reduction_intergeneration_cutoff)
{
        using LP = LOG_POLICY<LL>;

        // check for infected members and sort
        bool   infectious_cases;
        size_t num_cases;
        tie(infectious_cases, num_cases) = pool.SortMembers();

        if (!infectious_cases) {
                return;
        }

        // set up some stuff
        const auto  pType    = pool.m_pool_type;
        const auto  pImmune  = pool.m_index_immune;
        const auto& pMembers = pool.m_members;
        const auto  pSize    = pMembers.size();
        const auto  tProb    = transProfile.GetProbability();

        // match infectious and susceptible members, skip last part (immune members)
        for (size_t i_infected = 0; i_infected < num_cases; i_infected++) {
                // check if member is present today
                const auto p1 = pMembers[i_infected];
                if (!p1->IsInPool(pType)) {
                        continue;
                }
                auto& h1 = p1->GetHealth();
                if (h1.IsInfectious()) {
                        // loop over possible susceptible contacts
                        for (size_t i_contact = num_cases; i_contact < pImmune; i_contact++) {
                                // check if member is present today
                                const auto p2 = pMembers[i_contact];
                                if (!p2->IsInPool(pType)) {
                                        continue;
                                }
                                const double cProb_p1 = GetContactProbability(profile, p1, p2, pSize, pType,
                                		cnt_reduction_work, cnt_reduction_other,cnt_reduction_school, cnt_reduction_intergeneration,cnt_reduction_intergeneration_cutoff);
                                if (cHandler.HasContactAndTransmission(cProb_p1, tProb * p1->GetHealth().GetRelativeTransmission(p2->GetAge()))) {
                                        auto& h2 = p2->GetHealth();
                                        if (h1.IsInfectious() && h2.IsSusceptible()) {
                                                h2.StartInfection(h1.GetIdIndexCase(),p1->GetId());
                                                // No secondary infections with TIC; just mark p2 'recovered'
                                                if (TIC)
                                                        h2.StopInfection();
                                                LP::Trans(cLogger, p1, p2, pType, simDay, h1.GetIdIndexCase());
                                        }
                                }
                        }
                }
        }
}

//--------------------------------------------------------------------------
// All explicit instantiations.
//--------------------------------------------------------------------------
template class Infector<ContactLogMode::Id::None, false>;
template class Infector<ContactLogMode::Id::None, true>;
template class Infector<ContactLogMode::Id::Transmissions, false>;
template class Infector<ContactLogMode::Id::Transmissions, true>;
template class Infector<ContactLogMode::Id::All, false>;
template class Infector<ContactLogMode::Id::All, true>;


} // namespace stride
