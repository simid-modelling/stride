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
# R controller for the Stride model
#
#############################################################################

# load simid.rtools package
require(devtools,quietly = T)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools',quietly = T)

# LOAD R PACKAGES
# 
# XML         to parse and write XML files
# doParallel  to use parallel foreach
# ggplot2     to plot contact matrices
# gridExtra   to plot contact matrices
# mgcv        to sample uncertaint parameters from distributions
# data.table  to use convenience functions for range subsets (e.g. "between")
smd_load_packages(c('XML','doParallel','ggplot2','gridExtra','mgcv','data.table'))

# load general help functions
source('./bin/rstride/Misc.R')

# load specific functions
source('./bin/rstride/ContactInspector.R')
source('./bin/rstride/HealthEconomist.R')
source('./bin/rstride/LogParser.R')
source('./bin/rstride/IncidenceInspector.R')
source('./bin/rstride/SummaryInspector.R')
source('./bin/rstride/SurveyParticipantInspector.R')
source('./bin/rstride/TransmissionAnalyst.R')
source('./bin/rstride/TransmissionInspector.R')

#' Main function to run rStride for a given design of experiment
#' 
#' @param exp_design                vector with experimental design with parameter names as column names
#' @param dir_postfix               add postfix to output directory name (optional)
#' @param ignore_stdout             hide standard STRIDE terminal output   
#' @param remove_run_output         remove all run-specific output
#' @param parse_log_data            parse log files and aggregate into RData files
#' @param get_csv_output            store aggregated log data also in csv format
#' @param store_transmission_rdata  parse and store transmission data as RData file
#' @param use_date_prefix           add date tag as prefix to output directory and file names
run_rStride <- function(exp_design               = exp_design, 
                        dir_postfix              = '',
                        ignore_stdout            = TRUE, 
                        parse_log_data           = TRUE,
                        get_csv_output           = FALSE,
                        remove_run_output        = TRUE,
                        store_transmission_rdata = TRUE, 
                        use_date_prefix          = TRUE)
{
  
  # command line message
  smd_print('STARTING rSTRIDE CONTROLLER')

  #__________________________________#
  ## CHECK DESIGN OF EXPERIMENT   ####
  #__________________________________#
  if(.rstride$data_files_exist(exp_design) == FALSE ||
     .rstride$log_levels_exist(exp_design) == FALSE ||
     .rstride$valid_r0_values(exp_design)  == FALSE ||
     .rstride$valid_immunity_profiles(exp_design)  == FALSE ||
     .rstride$valid_seed_infected(exp_design) == FALSE){
    
    .rstride$cli_abort()
    return(.rstride$no_return_value())
  }
  
  #__________________________________#
  ## GENERAL OPTIONS              ####
  #__________________________________#
  stride_bin              <- './bin/stride'
  config_opt              <- '-c'
  config_default_filename <- './config/run_default.xml'
  output_dir              <- 'sim_output'
  
  #__________________________________#
  ## RUN TAG AND DIRECTORY        ####
  #__________________________________#

  # create run tag using the current time if use_date_prefix == TRUE
  run_tag <- ifelse(use_date_prefix,format(Sys.time(), format="%Y%m%d_%H%M%S"),'')
  
  # add dir_postfix
  run_tag <- paste0(run_tag,dir_postfix)
  
  # create project directory
  project_dir <- smd_file_path(output_dir,run_tag)

  # command line message
  smd_print('WORKING DIR',getwd())
  smd_print('PROJECT DIR',project_dir)
  
  #__________________________________#
  ## GENERAL CONFIG MODIFICATIONS ####
  #__________________________________#
  config_default                  <- xmlToList(config_default_filename)
  config_default$num_threads      <- 1
  config_default$vaccine_profile  <- 'None'
  config_default$vaccine_rate     <- 0
  config_default$immunity_profile <- 'None'
  config_default$immunity_rate    <- 0
  config_default$output_summary   <- 'true'
  config_default$run_tag          <- run_tag
  config_default$num_cea_samples  <- 1e4
  
  #__________________________________#
  ## PARALLEL SETUP               ####
  #__________________________________#
  smd_start_cluster(timeout = 360)
  
  #__________________________________#
  ## RUN                          ####
  #__________________________________#
  
  # command line message
  smd_print('READY TO RUN',nrow(exp_design),'EXPERIMENT(S)')
  
  # add an "experiment id" to the design of experiment matrix
  exp_design$exp_id <- 1:nrow(exp_design) 
  
  # create temporary directory to store experiment results
  project_dir_exp <- smd_file_path(project_dir,'exp_all')
  
  time_stamp_loop = Sys.time()
  i_exp=1
  # run all experiments (in parallel)
  par_out <- foreach(i_exp=1:nrow(exp_design),
                     .combine='rbind',
                     .packages=c('XML','simid.rtools'),
                     .export = c('.rstride','par_nodes_info'),
                     .verbose=FALSE) %dopar%
                     {  
                       
                       # create locacl copy of 'par_nodes_info'
                       #par_nodes_info <- .rstride$par_nodes_info
                       
                       # print progress (only slave1)
                       smd_print_progress(i_exp,nrow(exp_design),time_stamp_loop,par_nodes_info)

                       # create experiment tag
                       exp_tag <- .rstride$create_exp_tag(i_exp)

                       # copy default param
                       config_exp <-   config_default
                       
                       # add design parameters
                       for(i_param in 1:ncol(exp_design)){
                         config_exp[names(exp_design)[i_param]] <- exp_design[i_exp,i_param]
                       }  
                       
                       # update experiment output prefix
                       config_exp$output_prefix <- smd_file_path(project_dir,exp_tag,.verbose=FALSE)
                       
                       # create xml file
                       config_exp_filename <- .rstride$save_config_xml(config_exp,'run',config_exp$output_prefix)

                       # run stride (using the C++ Controller)
                       system(paste(stride_bin,config_opt,paste0('../',config_exp_filename)),ignore.stdout=ignore_stdout)

                       # load output summary
                       summary_filename <- file.path(config_exp$output_prefix,'summary.csv')
                       run_summary      <- read.table(summary_filename,header=T,sep=',')
                       
                       # merge output summary with input param
                       # note: do not use "merge" to prevent issues with decimal numbers
                       config_df   <- as.data.frame(config_exp)
                       config_df   <- config_df[,!names(config_df) %in% names(run_summary)]
                       run_summary <- cbind(run_summary,config_df)
                       
                       # if we do not wat to parse log data, return run summary
                       if(!parse_log_data){
                           return(run_summary)
                       }

                       ## PARSE LOGFILE
                       # create rstride_out list
                       rstride_out <- list()
                       
                       # parse contact_log (if present)
                       contact_log_filename <- smd_file_path(config_exp$output_prefix,'contact_log.txt')
                       if(file.exists(contact_log_filename)){
                         rstride_out <- .rstride$parse_contact_logfile(contact_log_filename,i_exp)
                       }
                       
                       # convert 'cases' file (if present) => "prevalence"
                       cases_filename <- smd_file_path(config_exp$output_prefix,'cases.csv')
                       if(file.exists(cases_filename)){
                         data_cases        <- read.table(cases_filename,sep=',')
                         names(data_cases) <- paste0('day',seq(length(data_cases))-1)
                         data_cases$exp_id <- config_exp$exp_id
                         #save(data_cases,file=file.path(config_exp$output_prefix,'data_prevalence.RData'))
                         rstride_out$data_prevalence <- data_cases
                         
                       } else {
                         rstride_out$data_prevalence = NA
                       }
                       
                       # help function to get the daily incidence
                       get_counts <- function(all_data,sim_day_max,output_col = "counts"){
                         
                         if(all(is.na(all_data))){
                           return( rep(0,sim_day_max))
                         }
                         
                         # get all bins (-0.5 to set the midpoint to 0,1,2,...)
                         breaks <- (0:max(all_data+1,na.rm=T))-0.5
                         
                         # get statistics
                         data_out <- unlist(hist(all_data,breaks,include.lowest = T,right=F,plot=F)[output_col])
                       
                         # limit to given number of days
                         data_out <- data_out[1:sim_day_max]
                           
                         return(data_out)
                       }
                       
                       # account for non-symptomatic cases
                       flag <- rstride_out$data_transmission$start_symptoms == rstride_out$data_transmission$end_symptoms
                       rstride_out$data_transmission$start_symptoms[flag] <- NA
                       
                       # save incidence
                       num_sim_days           <- config_exp$num_days
                       new_infections         <- get_counts(rstride_out$data_transmission$sim_day,num_sim_days)
                       new_infectious_cases   <- get_counts(rstride_out$data_transmission$sim_day + rstride_out$data_transmission$start_infectiousness,num_sim_days)
                       new_symptomatic_cases  <- get_counts(rstride_out$data_transmission$sim_day + rstride_out$data_transmission$start_symptoms,num_sim_days)
                       new_recovered_cases    <- get_counts(rstride_out$data_transmission$sim_day + rstride_out$data_transmission$end_symptoms,num_sim_days)
                       sim_day                <- get_counts(rstride_out$data_transmission$sim_day,num_sim_days,output_col = 'mids')
                       sim_date               <- as.Date(config_exp$start_date,'%Y-%m-%d') + sim_day
                       
                       rstride_out$data_incidence <- data.frame(sim_day               = sim_day,
                                                                sim_date              = sim_date,
                                                                new_infections        = new_infections,
                                                                new_infectious_cases  = new_infectious_cases,
                                                                new_symptomatic_cases = new_symptomatic_cases,
                                                                new_recovered_cases   = new_recovered_cases,
                                                                exp_id                = config_exp$exp_id,
                                                                row.names = NULL)
                       
                       # if transmission data should not be stored, replace item by NA
                       if(!store_transmission_rdata){
                         rstride_out$data_transmission <- NA
                       }
                       
                       # save list with all results
                       save(rstride_out,file=smd_file_path(project_dir_exp,paste0(exp_tag,'_parsed.RData')))
                       
                       # remove experiment output and config
                       if(remove_run_output){
                         unlink(config_exp$output_prefix,recursive=TRUE)
                         unlink(config_exp_filename,recursive = TRUE)
                       }
                       
                       # Finally: return experiment output summary
                       return(run_summary)
                     }
  
  # print final statement
  smd_print_progress(nrow(exp_design),nrow(exp_design),time_stamp_loop,par_nodes_info)
  
  # save overal summary
  write.table(par_out,file=file.path(project_dir,paste0(run_tag,'_summary.csv')),sep=',',row.names=F)
  
  #__________________________________#
  ## AGGREGATE OUTPUT             ####
  #__________________________________#
  # if log data is parsed => aggregate
  if(parse_log_data){
    .rstride$aggregate_compressed_output(project_dir,get_csv_output)
  }
  
    # remove project output
  if(remove_run_output){
    unlink(project_dir_exp,recursive = T)
  }
  
  
  #__________________________________#
  ## TERMINATE PARALLEL NODES     ####
  #__________________________________#
  smd_stop_cluster()
  
  # command line message
  smd_print('rSTRIDE CONTROLLER FINISHED')
  
  return(project_dir)
  
} # end run_rStride function
