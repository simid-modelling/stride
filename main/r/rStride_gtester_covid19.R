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
dir_postfix <- '_gtester'

################################## #
## DESIGN OF EXPERIMENTS        ####
################################## #

# uncomment the following line to inspect the config xml tags
#names(xmlToList('./config/run_default.xml'))

# set the number of realisations per configuration set
num_seeds  <- 5

# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(r0                            = 2.5,
                          num_days                      = 30,
                          rng_seed                      = seq(num_seeds),
                          num_participants_survey       = 10,   
                          num_infected_seeds            = 540,
                          disease_config_file           = 'disease_covid19_age.xml',
                          population_file               = 'pop_belgium600k_c500_teachers_censushh.csv',
                          age_contact_matrix_file       = 'contact_matrix_flanders_conditional_teachers.xml',
                          start_date                    = '2020-03-05',
                          holidays_file                 = 'holidays_flanders_2020.json',
                          school_system_adjusted        = 0,
                          telework_probability          = 0,
                          cnt_reduction_workplace       = 0,
                          cnt_reduction_other           = 0,
                          compliance_delay_workplace    = 0,
                          compliance_delay_other        = 0,
                          num_daily_imported_cases      = 0,
                          cnt_reduction_workplace_exit  = 0,
                          cnt_reduction_other_exit      = 0,
                          cnt_reduction_school_exit     = 0,
                          cnt_reduction_intergeneration = 0,
                          cnt_reduction_intergeneration_cutoff = 0,
                          cnt_intensity_householdCluster = 0,
                          detection_probability          = 0,
                          case_finding_efficency         = 0,
                          case_finding_capacity          = 0,
                          test_false_negative            = 0,
                          gtester_label                  = 'covid_all',
                          stringsAsFactors = F)

 
# daily seeding
exp_design_daily <- exp_design
exp_design_daily$num_daily_imported_cases <- 10
exp_design_daily$gtester_label            <- 'covid_daily'

# distancing
exp_design_dist <- exp_design
exp_design_dist$holidays_file              <- 'calendar_belgium_2020_covid19_april.json'
exp_design_dist$cnt_reduction_workplace    <- 0.3;
exp_design_dist$cnt_reduction_other        <- 0.4;
exp_design_dist$compliance_delay_workplace <- 3;
exp_design_dist$compliance_delay_other     <- 4;
exp_design_dist$gtester_label              <- 'covid_distancing'


# age_15min
exp_design_15min <- exp_design
exp_design_15min$disease_config_file     <- 'disease_covid19_age_15min.xml'
exp_design_15min$age_contact_matrix_file <- 'contact_matrix_flanders_conditional_teachers_15min.xml'
exp_design_15min$gtester_label           <- 'covid_15min'

# householdCluster
exp_design_hhcl <- exp_design
exp_design_hhcl$population_file       <- 'pop_belgium600k_c500_teachers_censushh_extended3_size2.csv'
exp_design_hhcl$cnt_intensity_householdCluster <- 4/7
exp_design_hhcl$holidays_file         <- 'calendar_belgium_2020_covid19_exit_school_adjusted.json'
exp_design_hhcl$start_date            <- '2020-06-01'

# contact tracing
exp_design_cts <- exp_design
exp_design_cts$detection_probability   <- 0.5
exp_design_cts$holidays_file           <- 'calendar_belgium_2020_covid19_exit_school_adjusted.json'
exp_design_cts$start_date              <- '2020-06-01'
exp_design_cts$case_finding_efficency  <- 0.7
exp_design_cts$test_false_negative     <- 0.1
exp_design_cts$case_finding_capacity   <- 1000

# rbind all designs
exp_design <- rbind(exp_design,exp_design_daily, exp_design_dist,
                    exp_design_15min, exp_design_hhcl, exp_design_cts)

# add a unique seed for each run
set.seed(125)
exp_design$rng_seed <- sample(nrow(exp_design))
dim(exp_design)


################################## #
## RUN rSTRIDE                  ####
################################## #
project_dir <- run_rStride(exp_design               = exp_design,
                           dir_postfix              = dir_postfix)


##################################### #
## EXPLORE INPUT-OUTPUT BEHAVIOR   ####
##################################### #
inspect_summary(project_dir)
inspect_participant_data(project_dir)
inspect_incidence_data(project_dir)
inspect_prevalence_data(project_dir)
inspect_transmission_dynamics(project_dir)
inspect_tracing_data(project_dir)


##################################### #
## CHECK INPUT-OUTPUT              ####
##################################### #

# terminal message
smd_print('START REGRESSION TEST')

## Load project summary 
project_summary <- .rstride$load_project_summary(project_dir)
project_summary$output_prefix  <- NULL
project_summary$run_tag        <- NULL
project_summary$run_time       <- NULL
project_summary$total_time     <- NULL

# load the incidence output
data_incidence     <- .rstride$load_aggregated_output(project_dir,'data_incidence')
dim(data_incidence)

# get all tracing output
data_prevalence <- .rstride$load_aggregated_output(project_dir,'data_prevalence_symptomatic')
dim(data_prevalence)

## Load reference data
ref_project_summary  <- readRDS(file='tests/regression_rstride_summary.rds')
ref_data_incidence   <- readRDS(file='tests/regression_rstride_incidence.rds')
ref_data_prevalence  <- readRDS(file='tests/regression_rstride_prevalence.rds')

## COMPARE SUMMARY
diff_summary    <- setdiff(project_summary,ref_project_summary)
if(length(diff_summary)>0){ 
  smd_print("SUMMARY CHANGED",WARNING = T)
  #print(head(diff_summary))
} else{
  smd_print("SUMMARY OK")
}

## COMPARE INCIDENCE
diff_incidence  <- setdiff(data_incidence,ref_data_incidence)
if(length(diff_incidence)>0){ 
  smd_print("INCIDENCE CHANGED",WARNING = T)
  #print(head(diff_incidence))
} else{
  smd_print("INCIDENCE OK")
}

## COMPARE PREVALENCE
diff_prevalence <- setdiff(data_prevalence,ref_data_prevalence)
if(length(diff_prevalence)>0){ 
  smd_print("PREVALENCE CHANGED",WARNING = T)
  #print(head(diff_prevalence))
} else{
  smd_print("PREVALENCE OK")
}

# terminal message
smd_print('REGRESSION TEST COMPLETE')

# short call for "reset reference values"
rrv <- function(){
  saveRDS(project_summary,file='tests/regression_rstride_summary.rds')
  saveRDS(data_incidence, file='tests/regression_rstride_incidence.rds')
  saveRDS(data_prevalence,file='tests/regression_rstride_prevalence.rds')
  smd_print('NEW REFERENCE VALES STORED: LOCAL')
}

# update the rstride reference values in the repo (note: local function for LW)
update_rrv_repo <- function(){
  stride_repo_dir <- 'tests'
  stride_repo_dir <- '~/Documents/university/research/stride/repo/stride_lw/main/resources/rstride_test'
  saveRDS(project_summary,file=file.path(stride_repo_dir,'regression_rstride_summary.rds'))
  saveRDS(data_incidence,file=file.path(stride_repo_dir,'regression_rstride_incidence.rds'))
  saveRDS(data_prevalence,file=file.path(stride_repo_dir,'regression_rstride_prevalence.rds'))
  smd_print('NEW REFERENCE VALES STORED: IN STRIDE REPOSITORY')
}




