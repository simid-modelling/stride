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
#  Copyright 2020, Willem L, Kuylen E & Broeckhove J
#############################################################################
#
# Call this script from the main project folder (containing bin, config, lib, ...)
# to get all relative data links right. 
#
# E.g.: path/to/stride $ ./bin/rStride_explore.R 
#
#############################################################################

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

# set directory postfix (optional)
dir_postfix <- '_intervention'

# store all transmission output
store_transmission_rdata <- FALSE

##################################
## DESIGN OF EXPERIMENTS        ##
##################################

# uncomment the following line to inspect the config xml tags
#names(xmlToList('./config/run_default.xml'))

# set the number of realisations per configuration set
num_seeds  <- 5

# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(r0                            = seq(3.5,3.5,0.1),
                          num_days                      = 50,
                          rng_seed                      = seq(num_seeds),
                          num_participants_survey       = 300,
                          seeding_rate                  = 30e-5, 
                          disease_config_file           = "disease_covid19_age.xml",
                          population_file               = "pop_belgium600k_c500_teachers_censushh.csv",
                          age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                          start_date                    = c('2020-02-22'),
                          #holidays_file                 = c("calendar_belgium_2020_covid19_may_workplace.json"),
                          holidays_file                 = c("calendar_belgium_2020_covid19_may_both.json"),
                          age_break_school_types        = c(12),
                          telework_probability          = c(0),
                          cnt_reduction_workplace       = c(0.7),
                          cnt_reduction_other           = c(0.7),
                          compliance_delay_workplace    = c(9),
                          compliance_delay_other        = c(14),
                          num_daily_imported_cases      = c(0),
                          cnt_reduction_workplace_exit  = 0,
                          cnt_reduction_other_exit      = 0,
                          cnt_reduction_intergeneration = c(0,0.95),
                          cnt_reduction_intergeneration_cutoff = 65,
                          detection_probability          = c(0.1,0.2,0.3),
                          case_finding_efficency         = 0.50,
                          case_finding_capacity          = c(2000),
                          stringsAsFactors = F)

# add a unique seed for each run
set.seed(125)
exp_design$rng_seed <- sample(nrow(exp_design))
dim(exp_design)

##################################
## RUN rSTRIDE                  ##
##################################
project_dir <- run_rStride(exp_design               = exp_design,
                           dir_postfix              = dir_postfix, 
                           store_transmission_rdata = store_transmission_rdata,
                           remove_run_output = TRUE,
                           ignore_stdout = TRUE)


#####################################
## EXPLORE INPUT-OUTPUT BEHAVIOR   ##
#####################################
inspect_summary(project_dir)


#####################################
## EXPLORE SURVEY PARTICIPANT DATA ##
#####################################
inspect_participant_data(project_dir)


##################################
## EXPLORE INCIDENCE DATA       ##
##################################
inspect_incidence_data(project_dir)



##################################
## EXPLORE TRANSMISSION         ##
##################################
inspect_transmission_data(project_dir)
 

##################################
## EXPLORE CONTACT TRACING      ##
##################################
inspect_tracing_data(project_dir)

 
