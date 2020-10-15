#!/usr/bin/env Rscript
############################################################################ #
#  This file is part of the Stride software. 
#  It is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by 
#  the Free Software Foundation, either version 3 of the License, or any 
#  later version.
#  The software is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  You should have received a copy of the GNU General Public License,
#  along with the software. If not, see <http://www.gnu.org/licenses/>.
#  see http://www.gnu.org/licenses/.
#
#
#  Copyright 2020, Willem L, Libin P
############################################################################ #
#
# Baseline settings for rStride intervention scenarios
#
############################################################################ #

################################## #
## DESIGN OF EXPERIMENTS        ####
################################## #

# get default parameter values to combine in a full-factorial grid
get_exp_param_default <- function(bool_child_param = FALSE, 
                                  bool_min_restrictive = FALSE,
                                  bool_revised_model_param = FALSE){
   
   # create calendar files
   create_calendar_files()
   
   out <- list(r0                            = seq(3.4,3.4,0.1),
                num_days                      = 196,
                num_seeds                     = 10,
                num_participants_survey       = 30,
                num_infected_seeds            = 510,
                disease_config_file           = "disease_covid19_age.xml",
                population_file               = c("pop_belgium11M_c500_teachers_censushh.csv"),
                age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                start_date                    = c('2020-02-17'),
                holidays_file                 = 'calendar_belgium_2020_covid19_exit_school_adjusted.csv',
                telework_probability          = 0,
                cnt_reduction_workplace       = 0.8,
                cnt_reduction_other           = 0.85,
                compliance_delay_workplace    = 6,
                compliance_delay_other        = 6,
                num_daily_imported_cases      = 0,
                cnt_reduction_workplace_exit  = seq(0.6,0.8,0.1),
                cnt_reduction_other_exit      = c(0.7,0.80,0.85),
                cnt_reduction_school_exit     = 0.5,
                cnt_reduction_intergeneration = 0.9,
                cnt_reduction_intergeneration_cutoff = 65,
                cnt_intensity_householdCluster = 0,
                detection_probability          = 0,
                tracing_efficiency_household   = 0.9, 
                tracing_efficiency_other       = 0.5,
                case_finding_capacity          = 10000, # no limit at this stage
                delay_isolation_index          = 1,
                delay_contact_tracing          = 3,
                test_false_negative            = 0.1,
               
                # log level
                event_log_level                 = "Transmissions",

                # factor for parameter estimation and fitting
                hosp_probability_factor        = 1,
               
                # unversal testing
                unitest_pool_allocation       = c("data/pop_belgium11M_c500_pool_allocation_$unitest_pool_size.csv"),
                unitest_fnr                   = c(0.01),
                unitest_n_tests_per_day       = 0, #c(25000),
                unitest_pool_size             = c(32),
                unitest_test_compliance       = c(0.9),
                unitest_isolation_compliance  = c(0.8),
               
               # hospital admissions
                hospital_category_age         = paste(0,19,60,80,sep=','),
                hospital_probability_age      = paste(0.049,0.03024,0.1197,0.5922,sep=','),
                hospital_mean_delay_age       = paste(3,7,7,6,sep=','),
                
               # threshold for log parsing (default is NA == no threshold)
               logparsing_cases_upperlimit    = NA
               
          )
   
    if(bool_revised_model_param){
      # relative proportions
      # reference: hospital survey data by age (faes et al) / observed sympt cases by age R0 callibration 2020-09-17
      out$hospital_probability_age      = paste(c(0.5863577,0.6193339,1.1223633,3.1063142)/3.1,collapse=',') # still requires rescaling
      out$hospital_mean_delay_age       = paste(3,7,6,4,sep=',')
      
      # disease history: literature based distributions
      out$disease_config_file <- 'disease_covid19_lognorm.xml'
      
      
      ## parameters from 20201015_142948_param6_d73_ensemble_parameter_pareto_incidence_single_mean
      out$r0 <- 3.45
      out$hosp_probability_factor <- 0.21 
      out$num_infected_seeds <- 306 
      out$cnt_reduction_workplace <- 0.85 
      out$cnt_reduction_other     <- 0.86 
      out$compliance_delay_other  <- 5
      out$compliance_delay_workplace <- 6 
      #out$num_seeds <- NA
      
   }
   
   # change parameters if childrens infectiousness is 1/2 compared to adults
   if(bool_child_param){ 
      # parameters from 20201007_100823_param5child_d73_2k_vh_parameter_pareto_incidence_single
      out$disease_config_file <- "disease_covid19_lognorm_child.xml"
      out$r0 <- 3.38
      out$hosp_probability_factor <- 0.19
      out$num_infected_seeds <- 259
      out$cnt_reduction_workplace <- 0.82
      out$cnt_reduction_other     <- 0.87
      out$compliance_delay_other  <- 7
      out$compliance_delay_workplace <- 7
   }
   
   # select least stringent social mixing assumptions
   if(bool_min_restrictive){
      out$cnt_reduction_workplace_exit <- min(out$cnt_reduction_workplace_exit)
      out$cnt_reduction_other_exit <- min(out$cnt_reduction_other_exit)
   }  
   # return parameters
   return(out)
}

