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

popSizes <- c('600k', '1000k', '3000k')
popSizesNumerical <- c(600000, 1000000, 3000000)

for (i in 1:length(popSizes)) {
  # set directory postfix (optional)
  dir_postfix <- paste0('popsizes_', popSizes[i])
  
  # set parameters
  num_seeds <- 50 # Number of runs per scenario

  exp_design <- expand.grid( # R0 between 1.5 and 3 (slightly broader range than reported by Zhao et al. 2020 Int J of Inf Dis)
                              r0                            = seq(1.5, 3, length.out=4),
                              # Run for approx. 3 months (February + March + April 2020)
                              num_days                      = 100,
                              rng_seed                      = seq(num_seeds),
                              num_participants_survey       = 0,
                              track_index_case              = 'false',
                              contact_log_level             = "Transmissions",
                              #seeding_rate                  = 1.7e-5, 
                              seeding_rate                  = 10 / popSizesNumerical[i],
                              disease_config_file           = "disease_covid19.xml",
                              population_file               = c(paste0("pop_belgium", popSizes[i], "_c500_teachers_censushh.csv")),
                              age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                              start_date                    = c('2020-02-01'),
                              holidays_file                 = "holidays_flanders_2020.json",
                              # No interventions
                              telework_probability          = c(0),
                              cnt_reduction_work            = c(0),
                              cnt_reduction_other           = c(0),
                              stringsAsFactors = F)
  # add a unique seed for each run
  set.seed(125)
  exp_design$rng_seed <- sample(nrow(exp_design))
  # dim(exp_design)
  
  # run rStride 
  project_dir <- run_rStride(exp_design               = exp_design,
                             dir_postfix              = dir_postfix, 
                             remove_run_output        = FALSE,
                             ignore_stdout            = TRUE,
                             parse_log_data           = TRUE,    # parse txt log-files and aggregate
                             get_csv_output           = TRUE,    # save aggregated log-data as csv files
                             store_transmission_rdata = TRUE,
                             use_date_prefix          = FALSE)
  

  
}

