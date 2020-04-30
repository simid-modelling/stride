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
dir_postfix <- 'geoclustering_random_25'
 
# store all transmission output
store_transmission_rdata <- TRUE
 
##################################
## DESIGN OF EXPERIMENTS        ##
##################################

# set the number of realisations per configuration set
num_seeds  <- 20 
 
# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(  
                            age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                            disease_config_file           = "disease_covid19_age.xml",
                            holidays_file                 = "calendar_belgium_2020_covid19_july.json",     # all measures in place until 30/6
                            num_daily_imported_cases      = c(0),
                            num_days                      = 150,
                            num_participants_survey       = 0,
                            population_file               = "pop_belgium3000k_c500_teachers_censushh.csv",
                            r0                            = 3.5,
                            rng_seed                      = seq(num_seeds),
                            seeding_rate                  = 3*1e-5, 
                            start_date                    = '2020-02-17',
                            
                            # Social distancing parameters
                            school_system_adjusted        = 1,
                            telework_probability          = c(0),
                            cnt_reduction_workplace       = c(0.8),
                            cnt_reduction_other           = c(0.85),
                            compliance_delay_workplace    = c(6),
                            compliance_delay_other        = c(6),
                            cnt_reduction_workplace_exit  = 0.8,           # from July, same behavior
                            cnt_reduction_other_exit      = 0.85,          # from July, same behavior
                            cnt_reduction_school_exit     = 1,             # no mixing in schools
                            cnt_reduction_intergeneration = 0.9,           # distancing with 'elderly' people (see next)
                            cnt_reduction_intergeneration_cutoff = 65,     # age break for 'elderly' (see previous)
                          
                            # Contact tracing parameters
                            detection_probability          = 0,            # no tracing (yet)
                            case_finding_efficency         = 0,            # no tracing (yet)
                            case_finding_capacity          = 0,            # no tracing (yet)
                            delay_contact_tracing          = 0,            # no tracing (yet)
                            delay_testing                  = 0,            # no tracing (yet)
                            test_false_negative            = 0,            # no tracing (yet)
                            
                            # Non-compliance parameters
                            
                            #non_compliance_type            = "Hotspots",
                            #non_compliance_hotspots_file   = "data/pop_belgium3000k_c500_teachers_censushh_non_compliers_by_exceedance_prob_100.xml",
                            non_compliance_type            = "Random",
                            num_non_compliers              = 342600, 
                            non_compliance_by_age_file     = "data/non_compliance_by_age.xml",
                            
                            stringsAsFactors = F)

# add a unique seed for each run
set.seed(125)
exp_design$rng_seed <- sample(nrow(exp_design))
# dim(exp_design)
 
##################################
## RUN rSTRIDE                  ##
##################################
project_dir <- run_rStride(exp_design               = exp_design,
                            dir_postfix              = dir_postfix, 
                            store_transmission_rdata = store_transmission_rdata,
                            remove_run_output = FALSE,
                           parse_log_data = FALSE,
                            ignore_stdout = TRUE,
                           use_date_prefix = FALSE)
