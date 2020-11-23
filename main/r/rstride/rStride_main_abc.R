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
# ABC controller for the Stride model
#
############################################################################ #

#' Main rStride function for ABC
# abc_function_param <- c(15,3.4,256,0.4,0.85,7.4,0.85,4.51)
run_rStride_abc <- function(abc_function_param)
{
  # define rng seed
  rng_seed <- abc_function_param[1]

  # add process-id-specific delay
  Sys.sleep(log(rng_seed,10))

  wd_start <- getwd()
  run_tag <- basename(getwd())
  setwd('../..')
  
  cat(run_tag,file='./sim_output/abc_out.txt',fill = T,append = T)
  cat(abc_function_param,file='./sim_output/abc_out.txt',fill = T,append = T)
  
  # load functions within parallel worker  
  source('./bin/rstride/rStride.R')

  remove_run_output = TRUE
  
  ################################## #
  ## GENERAL OPTIONS              ####
  ################################## #
  stride_bin              <- './bin/stride'
  config_opt              <- '-c'
  config_default_filename <- './config/run_default.xml'
  output_dir              <- 'sim_output'
  
  ################################## #
  ## OUTPUT DIRECTORY             ####
  ################################## #

  # create project directory
  project_dir <- smd_file_path(output_dir,run_tag)

  # get default config
  config_exp <- create_default_config(config_default_filename, run_tag)
  
  # add default parameters and values to combine in a full-factorial grid
  model_param_update <- readRDS(file.path('./sim_output',run_tag,'model_param_update.rds'))

  # add design parameters
  config_exp[names(model_param_update)] <- model_param_update
 
 
  
  # use given parameters
  config_exp$rng_seed                    <- rng_seed
  config_exp$r0                          <- abc_function_param[2]
  config_exp$num_infected_seeds          <- abc_function_param[3]
  config_exp$hosp_probability_factor     <- abc_function_param[4]
  config_exp$cnt_reduction_workplace     <- abc_function_param[5]
  config_exp$compliance_delay_workplace  <- abc_function_param[6]
  config_exp$cnt_reduction_other         <- abc_function_param[7]
  config_exp$compliance_delay_other      <- abc_function_param[8]
  
  # some input parameters need to be an integer value
  config_exp$num_infected_seeds         <- round(config_exp$num_infected_seeds )
  config_exp$compliance_delay_workplace <- round(config_exp$compliance_delay_workplace)
  config_exp$compliance_delay_other     <- round(config_exp$compliance_delay_other)
  
  ################################## #
  ## RUN                          ####
  ################################## #
  
  # create experiment tag
  i_exp   <- rng_seed
  exp_tag <- .rstride$create_exp_tag(i_exp)
  
   #save the config as XML file
   output_prefix       = smd_file_path(project_dir,exp_tag,.verbose=FALSE)
   config_exp$output_prefix <- output_prefix 
   
   config_exp_filename = paste0(output_prefix,".xml")
   save_config_xml(config_exp, config_exp_filename)

   # run stride (using the C++ Controller)
   cmd = paste(stride_bin,config_opt, paste0("../", config_exp_filename))
   system(cmd,ignore.stdout = TRUE)

   # load output summary
   summary_filename <- file.path(output_prefix,'summary.csv')
   run_summary      <- read.table(summary_filename,header=T,sep=',')
   
   # merge output summary with input param
   # note: do not use "merge" to prevent issues with decimal numbers
   config_df   <- as.data.frame(config_exp)
   config_df   <- config_df[,!names(config_df) %in% names(run_summary)]
   run_summary <- cbind(run_summary,config_df)
   
   # save summary
   write.table(run_summary,file=file.path(project_dir=output_prefix,
                                          paste0(run_tag,'_summary.csv')),sep=',',row.names=F)
   
   # parse log file if there is no log threshold (NULL or NA) OR if simulated cases < threshold 
   if(is.null(config_exp$logparsing_cases_upperlimit) ||
      is.na(config_exp$logparsing_cases_upperlimit) ||
      run_summary$num_cases < config_exp$logparsing_cases_upperlimit){
      
      # parse log output (and save as rds file)
      parse_log_file(config_exp, 
                     i_exp, 
                     get_burden_rdata=FALSE, 
                     get_transmission_rdata=FALSE, 
                     get_tracing_rdata = FALSE, 
                     project_dir_exp = output_prefix)
   } else{
      
      # reset wd
      setwd(wd_start)
      
      return(rep(0,(47+46+46)))
      #return(0)
   }
   
   print(120)  
   # ref data (temp)
   ## SERO-PREVALENCE DATA ----
   prevalence_ref <- load_observed_seroprevalence_data()

   ## DOUBLING TIME 3.1 (2.4-4.4) \cite{pellis2020challenges} ----
   sim_start_date <- as.Date(unique(run_summary$start_date))
   sim_ref_dates  <- seq(sim_start_date+10,sim_start_date+26,1)
   ref_doubling_time <- data.frame(dates = seq(as.Date('2020-02-24'),as.Date('2020-03-08'),1),
                                   mean = 3.1,
                                   CI_low = 2.4,
                                   CI_upper = 4.4
   )
   
   # get transmission output
   parsed_logfile <- dir(output_prefix,pattern = 'rds',full.names = T)
   parsed_logdata <- readRDS(parsed_logfile)
   names(parsed_logdata)
   data_incidence_all <- parsed_logdata$data_incidence
   print(139)
   # hospital admissions
   data_incidence <- data_incidence_all[data_incidence_all$sim_date >= as.Date('2020-03-15'),]
   
   # make sure all dates are in the dataset
   dummy_dates <- data.frame(sim_date=seq(as.Date('2020-03-15'),(as.Date(run_summary$start_date)+run_summary$num_days-1),1),
                    admissions_blank = 0)
   
   data_incidence <-  merge(data_incidence,dummy_dates,by='sim_date',all.y = T)
   
   # select and set NA to 0
   new_hospital_admissions <- data_incidence$new_hospital_admissions
   new_hospital_admissions[is.na(new_hospital_admissions)] <- 0
   
   # doubling time
   ref_dates                  <- ref_doubling_time$dates
   doubling_time_observed     <- mean(ref_doubling_time$mean)
print(146)
   data_incidence_all$doubling_time <- NA
   flag_exp_doubling   <- data_incidence_all$sim_date %in% ref_dates
   if(sum(flag_exp_doubling)>0){
     doubling_time_model     <- get_doubling_time(data_incidence_all$new_infections[flag_exp_doubling])
     doubling_time_model     <- as.numeric(doubling_time_model)
   } else{
      doubling_time_model <- NA
   }
   
   # FIX
   # make sure all dates are in the dataset
   dummy_dates <- data.frame(sim_date=seq(as.Date(run_summary$start_date),(as.Date(run_summary$start_date)+run_summary$num_days-1),1),
                             col_dummy = 0)
   data_infections <- data_incidence_all[,c('sim_date','new_infections')]
   data_infections <- merge(data_infections,dummy_dates,all.y=T)
   data_infections$cumulative_infections <- cumsum_na(data_infections$new_infections)
   
   # 2. sero-prevalence
   ref_dates                  <- prevalence_ref$seroprevalence_date
   total_incidence_observed   <- prevalence_ref$point_incidence_mean
   total_incidence_model      <- data_infections$cumulative_infections[data_infections$sim_date %in% ref_dates]
   # total_prevalence_model     <- total_incidence_model / run_summary$population_size
   total_prevalence_model     <- total_incidence_model #/ 1e11
   
   # set output
   abc_out <- c(new_hospital_admissions,   # length = 47
                rep(total_prevalence_model,23),    # length = 2
                rep(doubling_time_model,46))       # length = 1
   length(abc_out)
   
   # write.table(length(abc_out),paste0(output_prefix,'.csv'),sep=',',row.names=F)
   write.table((abc_out),paste0(output_prefix,'.csv'),sep=',',row.names=F)
   
   # remove experiment output and config
   if(remove_run_output){
     unlink(config_exp$output_prefix,recursive=TRUE)
     unlink(config_exp_filename,recursive = TRUE)
     unlink(paste0(output_prefix,'.csv'),recursive = T)
   }
  
   # reset wd
   setwd(wd_start)

  return(abc_out)
  
} # end run_rStride_abc function


