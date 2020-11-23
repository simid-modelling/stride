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
#  Copyright 2020, Willem L
############################################################################ #
#
# Call this script from the main project folder (containing bin, config, lib, ...)
# to get all relative data links right. 
#
# E.g.: path/to/stride $ ./bin/rStride_abc.R 
#
############################################################################ #

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  job_id <- paste0(format(Sys.time(), format="%Y%m%d_"),args[1])
} else{
  job_id <-''
}

# Clear work environment
rm(list=ls()[ls() != 'job_id'])

# Load rStride
source('./bin/rstride/rStride.R')

# Load default parameter configurations
source('./bin/rStride_intervention_baseline.R')

# load ABC package
library(EasyABC)

# set directory postfix (optional)
dir_postfix <- '_abc_param'

# create run tag using the current time if use_date_prefix == TRUE
# use_date_prefix <- TRUE
use_date_prefix <- job_id ==''
run_tag <- ifelse(use_date_prefix,format(Sys.time(), format="%Y%m%d_%H%M%S"),job_id)
run_tag_data <- paste0(run_tag,dir_postfix)

################################################ #
## DESIGN OF EXPERIMENTS  ----
################################################ #

# add default parameters and values to combine in a full-factorial grid
model_param_update <- get_exp_param_default(bool_revised_model_param = T,
                                            bool_min_restrictive = T)

# TEMP
model_param_update$population_file <- "pop_belgium600k_c500_teachers_censushh.csv"
model_param_update$num_days        <- 74
model_param_update$logparsing_cases_upperlimit <- 3.0e5


ref_period <- seq(as.Date('2020-03-15'),
                  as.Date(model_param_update$start_date) + model_param_update$num_days-1,
                  1)

################################################ #
## REFERENCE DATA  ----
################################################ #

## HOSPITAL REFERENCE DATA COVID-19 ----
# use (local version of) most recent SCIENSANO data (or backup version)
hosp_ref_data          <- get_observed_incidence_data()
hosp_ref_data$sim_date <- as.Date(hosp_ref_data$sim_date)
hosp_ref_data          <- hosp_ref_data[hosp_ref_data$sim_date %in% ref_period,]
dim(hosp_ref_data)


## SERO-PREVALENCE DATA ----
prevalence_ref <- load_observed_seroprevalence_data()

# select simulation period
sel_ref_dates  <- prevalence_ref$seroprevalence_date %in% ref_period
prevalence_ref <- prevalence_ref[sel_ref_dates,]

## DOUBLING TIME 3.1 (2.4-4.4) \cite{pellis2020challenges} ----
ref_doubling_time <- data.frame(dates = seq(as.Date('2020-02-24'),as.Date('2020-03-08'),1),
                                mean = 3.1,
                                CI_low = 2.4,
                                CI_upper = 4.4
)


sum_stat_obs=c(hosp_ref_data$hospital_admissions,            # 47
               rep(prevalence_ref$point_incidence_mean,23),  # 2     rep 23x
               rep(mean(ref_doubling_time$mean),46))         # 1     rep 46x

names(sum_stat_obs) <- c(paste0('hosp_adm',1:length(hosp_ref_data$hospital_admissions)),
                         paste0('incidence',1:2),
                         rep('inc_duplicate',44),
                         'init_doubling_time',
                         rep('dtime_duplicate',45))


################################## #
## RUN ABC  ----
################################## #

# set priors
stride_prior <- list(r0                         = c("unif",1.0,5.0),   
                     num_infected_seeds         = c("unif",200,600),
                     hosp_probability_factor    = c("unif",0.05,0.95),
                     cnt_reduction_workplace    = c("unif",0.60,0.95),
                     compliance_delay_workplace = c("unif",4.51,7.49),  # rounded: 5-7
                     cnt_reduction_other        = c("unif",0.60,0.95),
                     compliance_delay_other     = c("unif",4.51,7.49))  # rounded: 5-7


# set samples and cluster size
n_sample = 24
n_cluster = 8

## for debugging
#.rstride$set_wd()                    

# create output folder and set workdir
run_file_path <- dirname(smd_file_path('./sim_output',run_tag_data,'test'))
setwd(run_file_path)
saveRDS(model_param_update,'model_param_update.rds')

# stride_out <- run_rStride_abc(c(20,4,400,0.4,0.85,7.4,0.85,4.51))
# length(stride_out)
# length(sum_stat_obs)

# p = 0.2
# ABC_stride<-ABC_rejection(model     = run_rStride_abc,
#                            prior    = stride_prior,
#                            nb_simul = n_sample,
#                            summary_stat_target=sum_stat_obs,
#                            tol=p,
#                            verbose = T,
#                            n_cluster=n_cluster,
#                            use_seed=TRUE,
#                            progress_bar=T)


pacc=0.5
ABC_stride<-ABC_sequential(model=run_rStride_abc,
                           prior=stride_prior,
                           nb_simul=n_sample,
                           summary_stat_target=sum_stat_obs,
                           method = "Lenormand",
                           p_acc_min=pacc,
                           verbose = T,
                           n_cluster=n_cluster,
                           use_seed=TRUE,
                           progress_bar=T)

# set back workdir
setwd('../..')

# par(mfrow=c(3,2))
saveRDS(ABC_stride,file=smd_file_path('./sim_output',run_tag_data,'ABC_stride.rsd'))

############################# #
## EXPLORE RESULTS         ####
############################# #

plot_abc_results <- function(ABC_out){
  
  # parameters
  for(i in 1:ncol(ABC_out$param)){
    hist(ABC_out$param[,i],20,
         xlim = as.numeric(stride_prior[[i]][-1]),
         xlab = names(stride_prior)[i])
  }
  
  # hospital incidence
  plot(hosp_ref_data$sim_date,
       hosp_ref_data$hospital_admissions,ylim=range(0,hosp_ref_data$hospital_admissions),
       xlab='',
       ylab='Hospital admissions')
  for(i in 1:nrow(ABC_out$stats)){
    lines(hosp_ref_data$sim_date,
          ABC_out$stats[i,grepl('hosp',names(sum_stat_obs))],
          col=alpha(4,0.5))
  }
  
  # initial doubling time
  x_lim <- range(0,pretty(sum_stat_obs[grepl('doubling',names(sum_stat_obs))]*1.1))
  hist(ABC_out$stats[,grepl('doubling',names(sum_stat_obs))],40,
       xlim=x_lim,
       xlab='Initial doubling time',
       main='')
  abline(v=mean(ref_doubling_time$mean),col=2)
  
  # total incidence
  x_lim <- range(0,pretty(sum_stat_obs[grepl('incidence',names(sum_stat_obs))]*1.1))
  hist(ABC_out$stats[,grepl('incidence',names(sum_stat_obs))][,1],20,xlim=x_lim,xlab='cummulative incidence 1')
  abline(v=sum_stat_obs[grepl('incidence',names(sum_stat_obs))][1],col=2)
  
  hist(ABC_out$stats[,grepl('incidence',names(sum_stat_obs))][,2],20,xlim=x_lim,xlab='cummulative incidence 2')
  abline(v=sum_stat_obs[grepl('incidence',names(sum_stat_obs))][2],col=2)
}

# open pdf stream
pdf(file=smd_file_path('./sim_output',run_tag_data,'results_ABC.pdf'))

# plot (final) results
par(mfrow=c(3,2))
plot_abc_results(ABC_stride)

# close pdf stream
dev.off()

# if intermediate results present ==>> plot
if( 'intermediary' %in% names(ABC_stride)){
  
  pdf(file=smd_file_path('./sim_output',run_tag_data,'results_ABC_intermediate.pdf'))
  par(mfrow=c(3,2))
  for(i_seq in 1:length(ABC_stride$intermediary)){
    ABC_stride_temp <- ABC_stride
    ABC_stride_temp$param <- ABC_stride$intermediary[[i_seq]]$posterior[,2:3]
    ABC_stride_temp$stats <- ABC_stride$intermediary[[i_seq]]$posterior[,-(1:3)]
    plot_abc_results(ABC_stride_temp)
  }
  
  dev.off()
}

## debug
if(0==1){
  
  abc_out <- read.table(smd_file_path('./sim_output',run_tag_data,'output'),sep=' ')
  dim(abc_out)
  length(sum_stat_obs)
  
  abc_out[,1]
  head(abc_out)
  abc_out$V55
  abc_out$V54
  abc_out$V146
  
}


