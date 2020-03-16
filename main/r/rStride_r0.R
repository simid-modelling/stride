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
# E.g.: path/to/stride $ ./bin/rStride_r0.R 
#
#############################################################################

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

# set directory postfix (optional)
dir_postfix <- '_r0'

##################################
## DESIGN OF EXPERIMENTS        ##
##################################

# uncomment the following line to inspect the config xml tags
#names(xmlToList('./config/run_default.xml'))

# set the number of realisations per configuration set
num_seeds  <- 4

# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(r0                            = seq(1,5,length=15),
                          num_days                      = c(20),
                          rng_seed                      = seq(num_seeds),
                          start_date                    = c('2017-01-01'),#,'2017-01-02','2017-01-03','2017-01-04','2017-01-05','2017-01-06','2017-01-07'),
                          track_index_case              = 'false',
                          contact_log_level             = 'Transmissions',
                          num_threads                   = 1,
                          seeding_rate                  = 1.7e-5,
                          seeding_age_min               = 1,
                          seeding_age_max               = 99,
                          disease_config_file           = "disease_covid19.xml",
                          population_file               = "pop_belgium600k_c500_teachers_censushh.csv",
                          age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                          adaptive_symptomatic_behavior = 'true',
                          stringsAsFactors = F)

# add a unique seed for each run
set.seed(num_seeds)
exp_design$rng_seed <- sample(nrow(exp_design))

##################################
## RUN rSTRIDE                  ##
##################################
project_dir <- run_rStride(exp_design,dir_postfix)


##################################
## REPRODUCTION NUMBER          ##
##################################
analyse_transmission_data_for_r0(project_dir)

