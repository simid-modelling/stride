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
# E.g.: path/to/stride $ ./bin/rStride_param.R 
#
############################################################################ #

# Clear work environment
rm(list=ls())

library(lhs)

# Load rStride
source('./bin/rstride/rStride.R')

# Load default parameter configurations
source('./bin/rStride_intervention_baseline.R')

# set directory postfix (optional)
dir_postfix <- '_int'

################################## #
## DESIGN OF EXPERIMENTS        ####
################################## #

# set number of experiments
num_experiments     <- 32

# add default parameters and values to combine in a LHS
exp_param_list <- get_exp_param_default(bool_min_restrictive = T,
                                        bool_revised_model_param = T)

# change parameters and values to combine in a full-factorial grid
# exp_param_list$population_file <- 'pop_belgium600k_c500_teachers_censushh.csv'
 exp_param_list$num_days <- 40      #74 # 150
 exp_param_list$r0 <- c(1.5,4)
 exp_param_list$logparsing_cases_upperlimit <- 3e5
 exp_param_list$hosp_probability_factor <- c(0.05,0.7)
 exp_param_list$num_infected_seeds <- c(210,560)
 # exp_param_list$num_seeds <- 2


################################################ #
## GENERATE DESIGN OF EXPERIMENT GRID         ####
################################################ #

# get number of values per parameter
num_param_values <- unlist(lapply(exp_param_list,length))

# select the parameters with at least 2 values
sel_param     <- names(num_param_values[num_param_values>1])

# setup latin hypercube design (and add parameter names)
lhs_design <- data.frame(randomLHS(num_experiments,length(sel_param)))
names(lhs_design) <- sel_param

# rescale LHS to given parameter range
for(i_param in sel_param){
  param_range <- range(exp_param_list[i_param])
  lhs_design[,i_param] <- lhs_design[,i_param] * diff(param_range) + param_range[1]
}

# copy lhs design into 'exp_design' and add other parameters
exp_design <- lhs_design
exp_param_names <- names(exp_param_list)
exp_param_names <- exp_param_names[!exp_param_names %in% sel_param]
i_param = exp_param_names[1]
for(i_param in exp_param_names){
  exp_design[,i_param] <- exp_param_list[i_param]
}


# fix for parameters that are a (non-decimal) number
sel_param_num <- names(exp_design)[grepl('num',names(exp_design))]
exp_design[,sel_param_num] <- round(exp_design[,sel_param_num])

# add a unique seed for each run
set.seed(125)
exp_design$rng_seed <- sample(nrow(exp_design))
dim(exp_design)

# check period
range(as.Date(exp_param_list$start_date), as.Date(exp_param_list$start_date)+ exp_param_list$num_days)

#exp_design <- exp_design[sample(nrow(exp_design),20),]
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


########################################### #
## PARAMETER ESTIMATION (optional)       ####
########################################### #
estimate_parameters(project_dir)


 
