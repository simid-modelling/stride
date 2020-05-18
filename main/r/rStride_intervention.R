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
#  Copyright 2020, Willem L, Kuylen E & Broeckhove J
############################################################################ #
#
# Call this script from the main project folder (containing bin, config, lib, ...)
# to get all relative data links right. 
#
# E.g.: path/to/stride $ ./bin/rStride_explore.R 
#
############################################################################ #

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

# set directory postfix (optional)
dir_postfix <- '_int'

################################## #
## DESIGN OF EXPERIMENTS        ####
################################## #

# uncomment the following line to inspect the config xml tags
#names(xmlToList('./config/run_default.xml'))

# set the number of realisations per configuration set
num_seeds  <- 10

# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(r0                            = seq(3.4,3.4,0.1),
                          num_days                      = 196,
                          rng_seed                      = seq(num_seeds),
                          num_participants_survey       = 300,
                          num_infected_seeds            = 750,
                          disease_config_file           = "disease_covid19_age.xml",
                          population_file               = c("pop_belgium11M_c500_teachers_censushh.csv"),
                          age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                          start_date                    = c('2020-02-17'),
                          holidays_file                 = 'calendar_belgium_2020_covid19_exit_school_adjusted.json',
                          school_system_adjusted        = 1,
                          telework_probability          = c(0),
                          cnt_reduction_workplace       = c(0.8),
                          cnt_reduction_other           = c(0.85),
                          compliance_delay_workplace    = c(6),
                          compliance_delay_other        = c(6),
                          num_daily_imported_cases      = c(0),
                          cnt_reduction_workplace_exit  = seq(0.2,0.6,0.2),
                          cnt_reduction_other_exit      = seq(0.7,0.8,0.1),
                          cnt_reduction_school_exit     = 0.5,
                          cnt_reduction_intergeneration = 0.9,
                          cnt_reduction_intergeneration_cutoff = 65,
                          detection_probability          = 0.5,
                          case_finding_efficency         = 0.7,
                          case_finding_capacity          = 10000, # no limit at this stage
                          delay_contact_tracing          = 3,
                          delay_testing                  = 1,
                          test_false_negative            = 0.1,
                          stringsAsFactors = F)

# check period
range(as.Date(exp_design$start_date), as.Date(exp_design$start_date)+ exp_design$num_days)

# add a unique seed for each run
set.seed(125)
exp_design$rng_seed <- sample(nrow(exp_design))
dim(exp_design)

################################## #
## RUN rSTRIDE                  ####
################################## #
project_dir <- run_rStride(exp_design               = exp_design,
                           dir_postfix              = dir_postfix)


############################# #
## INPUT-OUTPUT BEHAVIOR   ####
############################# #
inspect_summary(project_dir)


############################# #
## SURVEY PARTICIPANT DATA ####
############################# #
inspect_participant_data(project_dir)


############################# #
## INCIDENCE DATA          ####
############################# #
inspect_incidence_data(project_dir)


############################# #
## PREVALENCE              ####
############################# #
inspect_prevalence_data(project_dir)


############################# #
## TRANSMISSION            ####
############################# #
inspect_transmission_dynamics(project_dir)
 

############################# #
## CONTACT TRACING         ####
############################# #
inspect_tracing_data(project_dir)



 
