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
# E.g.: path/to/stride $ ./bin/rStride_contacts.R 
#
#############################################################################

# Clear work environment
rm(list=ls())

# load rStride
source('./bin/rstride/rStride.R')

##################################
## DESIGN OF EXPERIMENTS        ##
##################################

# uncomment the following line to inspect the config xml tags
#names(xmlToList('./config/run_default.xml'))

# set the number of realisations per configuration set
num_seeds  <- 1

# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(dir_postfix               = '_cnt',
                          contact_log_level         = "All",
                          num_days                  = 1,
                          seeding_rate              = 1.7e-5,
                          num_participants_survey   = 3000,
                          start_date                = c("2020-03-29","2020-03-30","2020-04-06",'2020-02-25'),
                          rng_seed                  = 1:num_seeds,
                          disease_config_file       = "disease_covid19.xml", 
                          population_file           = "pop_belgium600k_c500_teachers_censushh.csv",
                          age_contact_matrix_file   = "contact_matrix_flanders_conditional_teachers.xml",
                          holidays_file             = "holidays_flanders_2020.json",
                          telework_probability          = c(0),
                          cnt_reduction_work            = c(0.40),
                          cnt_reduction_other           = c(0.6),
                          compliance_delay              = c(1),
                          num_daily_imported_cases      = c(0),
                          stringsAsFactors = F)

# add a unique seed for each run
set.seed(125)
exp_design$rng_seed <- sample(nrow(exp_design))

##################################
## RUN rSTRIDE                  ##
##################################
project_dir <- run_rStride(exp_design  = exp_design,
                           dir_postfix = unique(exp_design$dir_postfix),
                           ignore_stdout = FALSE)


#####################################################
## EXPLORE SOCIAL CONTACT PATTERNS                 ##
#####################################################
inspect_contact_data(project_dir)


