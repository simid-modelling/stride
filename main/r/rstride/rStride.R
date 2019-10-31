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
smd_load_packages(c('XML','doParallel','ggplot2','gridExtra','mgcv'))

# load general help functions
source('./bin/rstride/Misc.R')

# load specific functions
source('./bin/rstride/ContactInspector.R')
source('./bin/rstride/HealthEconomist.R')
source('./bin/rstride/LogParser.R')
source('./bin/rstride/SummaryInspector.R')
source('./bin/rstride/SurveyParticipantInspector.R')
source('./bin/rstride/TransmissionAnalyst.R')
source('./bin/rstride/TransmissionInspector.R')

# Function to run rStride for a given design of experiment
run_rStride <- function(design_of_experiment = exp_design , dir_postfix = '',
                        ignore_stride_stdout = TRUE, remove_tmp_output = TRUE)
{
  
  # command line message
  smd_print('STARTING rSTRIDE CONTROLLER')

  ################################
  ## CHECK DESIGN OF EXPERIMENT ##
  ################################
  if(.rstride$data_files_exist(design_of_experiment) == FALSE ||
     .rstride$log_levels_exist(design_of_experiment) == FALSE ||
     .rstride$valid_r0_values(design_of_experiment)  == FALSE ||
     .rstride$valid_immunity_profiles(design_of_experiment)  == FALSE ||
     .rstride$valid_seed_infected(design_of_experiment) == FALSE){
    
    .rstride$cli_abort()
    return(.rstride$no_return_value())
  }
  
  ################################
  ## GENERAL OPTIONS            ##
  ################################
  stride_bin              <- './bin/stride'
  config_opt              <- '-c'
  config_default_filename <- './config/run_default.xml'
  output_dir              <- 'sim_output'
  
  ################################
  ## RUN TAG AND DIRECTORY      ##
  ################################
  
  # create run tag using the current time
  run_tag <- format(Sys.time(), format="%Y%m%d_%H%M%S")
  
  # add dir_postfix
  run_tag <- paste0(run_tag,dir_postfix)
  
  # create project directory
  project_dir <- smd_file_path(output_dir,run_tag)

  # command line message
  smd_print('WORKING DIR',getwd())
  smd_print('PROJECT DIR',project_dir)
  
  ##################################
  ## GENERAL CONFIG MODIFICATIONS ##
  ##################################
  config_default                  <- xmlToList(config_default_filename)
  config_default$num_threads      <- 1
  config_default$vaccine_profile  <- 'None'
  config_default$vaccine_rate     <- 0
  config_default$immunity_profile <- 'None'
  config_default$immunity_rate    <- 0
  config_default$output_summary   <- 'true'
  config_default$run_tag          <- run_tag
  config_default$num_cea_samples  <- 1e4
  
  
  ################################
  ## PARALLEL SETUP             ##
  ################################
  smd_start_cluster(timeout = 360)
  
  ##################################
  ## RUN                          ##
  ##################################
  
  # command line message
  smd_print('READY TO RUN',nrow(design_of_experiment),'EXPERIMENT(S)')
  
  # add an "experiment id" to the design of experiment matrix
  design_of_experiment$exp_id <- 1:nrow(design_of_experiment) 
  
  # create temporary directory to store experiment results
  project_dir_exp <- smd_file_path(project_dir,'exp_all')
  
  time_stamp_loop = Sys.time()
  # run all experiments (in parallel)
  par_out <- foreach(i_exp=1:nrow(design_of_experiment),
                     .combine='rbind',
                     .packages=c('XML','simid.rtools'),
                     .export = c('.rstride','par_nodes_info'),
                     .verbose=FALSE) %dopar%
                     {  
                       
                       # create locacl copy of 'par_nodes_info'
                       #par_nodes_info <- .rstride$par_nodes_info
                       
                       # print progress (only slave1)
                       smd_print_progress(i_exp,nrow(design_of_experiment),time_stamp_loop,par_nodes_info)

                       # create experiment tag
                       exp_tag <- .rstride$create_exp_tag(i_exp)

                       # copy default param
                       config_exp <-   config_default
                       
                       # add design parameters
                       for(i_param in 1:ncol(design_of_experiment)){
                         config_exp[names(design_of_experiment)[i_param]] <- design_of_experiment[i_exp,i_param]
                       }  
                       
                       # update experiment output prefix
                       config_exp$output_prefix <- smd_file_path(project_dir,exp_tag,.verbose=FALSE)
                       
                       # create xml file
                       config_exp_filename <- .rstride$save_config_xml(config_exp,'run',config_exp$output_prefix)

                       # run stride (using the C++ Controller)
                       system(paste(stride_bin,config_opt,paste0('../',config_exp_filename)),ignore.stdout=ignore_stride_stdout)

                       # load output summary
                       summary_filename <- file.path(config_exp$output_prefix,'summary.csv')
                       run_summary      <- read.table(summary_filename,header=T,sep=',')
                       
                       # merge output summary with input param
                       config_df   <- as.data.frame(config_exp)
                       run_summary <- merge(run_summary,config_df)
                       
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
                       
                       # save list with all results
                       save(rstride_out,file=smd_file_path(project_dir_exp,paste0(exp_tag,'_parsed.RData')))
                       
                       # remove experiment output and config
                       if(remove_tmp_output){
                         unlink(config_exp$output_prefix,recursive=T)
                         unlink(config_exp_filename,recursive = T)
                       }
                       
                       # return experiment output summary
                       return(run_summary)
                     }
  
  # print final statement
  smd_print_progress(nrow(design_of_experiment),nrow(design_of_experiment),time_stamp_loop,par_nodes_info)
  
  # save overal summary
  write.table(par_out,file=file.path(project_dir,paste0(run_tag,'_summary.csv')),sep=',',row.names=F)
  
  ###############################
  ## AGGREGATE OUTPUT          ##
  ###############################
  .rstride$aggregate_compressed_output(project_dir)
  
  
  # remove project output
  if(remove_tmp_output){
    unlink(project_dir_exp,recursive = T)
  }
  
  
  ###############################
  ## TERMINATE PARALLEL NODES  ##
  ###############################
  smd_stop_cluster()
  
  # command line message
  smd_print('rSTRIDE CONTROLLER FINISHED')
  
  return(project_dir)
  
} # end run_rStride function
