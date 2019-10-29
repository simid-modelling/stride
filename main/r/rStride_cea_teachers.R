#!/usr/bin/env Rscript
#############################################################################
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
#  Copyright 2019, Willem L, Kuylen E & Broeckhove J
#############################################################################
#
# Call this script from the main project folder (containing bin, config, lib, ...)
# to get all relative data links right. 
#
# E.g.: path/to/stride $ ./bin/rStride_reactive.R 
#
#############################################################################

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

# set directory postfix (optional)
dir_postfix <- '_cea'

#################################################
## DESIGN OF EXPERIMENTS                       ##
#################################################

# uncomment the following line to inspect the config xml tags
#names(XML::xmlToList('./config/run_default.xml'))

# set the number of realizations per configuration set
num_seeds  <- 10

# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(r0                         = seq(9,11,2),
                          num_days                   = 360,
                          rng_seed                   = seq(num_seeds),
                          num_participants_survey    = 3000,
                          track_index_case           = 'false',
                          contact_log_level          = 'Transmissions',
                          seeding_rate               = 1.7e-5,
                          disease_config_file        = 'disease_measles_adaptive_behavior.xml',
                          population_file            = 'pop_belgium600k_c500_teachers_censushh.csv',
                          age_contact_matrix_file    = 'contact_matrix_flanders_conditional_teachers.xml',
                          adaptive_symptomatic_behavior = 'true',
                          immunity_profile              = 'AgeDependent',           # 'None', 'Random', 'AgeDependent'
                          immunity_distribution_file    = 'data/immunity_measles_belgium.xml',
                          immunity_link_probability     = 0,
                          vaccine_profile               = c('Random','Teachers'),                 # 'None', 'Random', 'AgeDependent'
                          vaccine_rate                  = c(0.0,0.6),           # to be used with 'Random'
                          vaccine_min_age               = c(20),                 # to be used with 'Random'
                          vaccine_max_age               = NA,                       # to be used with 'Random'
                          case_detection_probability    = 0,                        # Enable case finding
                          num_cea_samples               = 1e4,
                          stringsAsFactors = F)

# remove doubles for 'no vaccination'
flag_to_remove <- exp_design$vaccine_rate == 0 &
                  (exp_design$vaccine_profile != exp_design$vaccine_profile[1] | 
                  exp_design$vaccine_min_age != exp_design$vaccine_min_age[1])
exp_design <- exp_design[!flag_to_remove,]


# adjust the ages of the target group, according the projected year
flag_add_max_age <- is.na(exp_design$vaccine_max_age)
exp_design$vaccine_max_age[flag_add_max_age] <- exp_design$vaccine_min_age[flag_add_max_age] + 10

# add a unique seed for each run
set.seed(num_seeds)
exp_design$rng_seed <- sample(nrow(exp_design))


#################################################
## RUN rSTRIDE                                 ##
#################################################
project_dir <- run_rStride(exp_design,dir_postfix)


#################################################
## EXPLORE SUMMARY                             ##
#################################################
inspect_summary(project_dir)


#################################################
## EXPLORE SURVEY PARTICIPANT DATA             ##
#################################################
inspect_participant_data(project_dir)


#################################################
## EXPLORE TRANSMISSION & OUTBREAKS            ##
#################################################
inspect_transmission_data(project_dir)


#################################################
## PERFORM COST EFFECTIVENESS ANALYSIS         ##
#################################################
calculate_cost_effectiveness(project_dir)

