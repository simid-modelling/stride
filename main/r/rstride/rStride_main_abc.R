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
#abc_function_param <- c(41,4,400,0.4,0.85,7.4,0.85,4.51)

################################################ #
## RUN  ----
################################################ #

run_rStride_abc <- function(abc_function_param,
                            remove_run_output     = TRUE)
{
  # define rng seed
  rng_seed <- abc_function_param[1]

  # add process-id-specific delay 
  # note: to prevent multiple project-dirs when starting many parallel processes
  Sys.sleep(log(rng_seed,10))

  wd_start <- getwd()
  run_tag <- basename(getwd())
  setwd('../..')

  # load functions within parallel worker  
  source('./bin/rstride/rStride.R')

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
   
   # parse event_log and process model output
   parse_log_file(config_exp, 
                  i_exp, 
                  # get_burden_rdata, 
                  get_transmission_rdata = FALSE, 
                  get_tracing_rdata = FALSE, 
                  project_dir_exp = config_exp$output_prefix,
                  bool_transmission_all = FALSE)
   
   # get transmission output
   parsed_logfile <- dir(output_prefix,pattern = 'rds',full.names = T)
   data_incidence_all <- readRDS(parsed_logfile)$data_incidence
   
   names(data_incidence_all)
  
   # get ref data (temp)
   sim_stat_filename <- file.path('./sim_output',run_tag,'sum_stat_obs.rds')
   if(file.exists(file.path('./sim_output',run_tag,'sum_stat_obs.rds'))){
      sum_stat_obs <- readRDS(file.path('./sim_output',run_tag,'sum_stat_obs.rds'))
   } else{
      ref_period   <- unique(data_incidence_all$sim_date)
      sum_stat_obs <- get_abc_reference_data(ref_period)
   }
   
   sum_stat_obs$date
   head(sum_stat_obs)
   table(sum_stat_obs$category)
   range(sum_stat_obs$date)
   
   # if doubling time is part of the reference output ==> add summary statistic for given period
   if(any(grepl('doubling_time',sum_stat_obs$category))){
      rstride_category <- as.character(unique(sum_stat_obs$category[grepl('doubling_time',sum_stat_obs$category)]))
      if(length(rstride_category)>1){
         smd_print('rSTRIDE CANNOT HANDLE MORE THAN ONE REFERENCE DOUBLING TIME (yet) => RETURN NA!',WARNING = T)
         data_incidence_all[,eval(rstride_category):=NA]
         #data_incidence_all[,get(rstride_category)]
      } else{
         
         dtime_period <- unique(sum_stat_obs$date[grepl('doubling_time',sum_stat_obs$category)])
         flag_exp_doubling   <- data_incidence_all$sim_date %in% dtime_period
         if(sum(flag_exp_doubling)>0){
            doubling_time_model     <- get_doubling_time(data_incidence_all$new_infections[flag_exp_doubling])
         } else{
            doubling_time_model <- NA
         }
         data_incidence_all[,eval(rstride_category):=doubling_time_model]
      }
      names(data_incidence_all)
   }
   
   # create model-based summary stats
   abc_out <- vector(length=nrow(sum_stat_obs))
   
   i_ref <- 50
   sum_stat_obs[i_ref,]
   for(i_ref in 1:nrow(sum_stat_obs)){
      
      # category and age ==>> column
      flag_col <- names(data_incidence_all) == sum_stat_obs$category[i_ref]
      table(flag_col)
      name_col <- names(data_incidence_all)[flag_col]
      
      # date => row
      data_incidence_all[,get(name_col)]
      data_incidence_all[sim_date == sum_stat_obs$date[i_ref],]
      
      # save value
      abc_out[i_ref] <- data_incidence_all[sim_date == sum_stat_obs$date[i_ref],get(name_col)]
   }
   head(abc_out)

   # tmp debug code
   #write.table((abc_out),paste0(output_prefix,'.csv'),sep=',',row.names=F)
   
   # remove experiment output and config
   if(remove_run_output){
     unlink(config_exp$output_prefix,recursive=TRUE)
     unlink(config_exp_filename,recursive = TRUE)
     #unlink(paste0(output_prefix,'.csv'),recursive = T) # tmp debug code
   }
  
   # reset wd
   setwd(wd_start)

  return(abc_out)
  
} # end run_rStride_abc function

################################################ #
## EXPLORE RESULTS  ----
################################################ #

# function to plot the ABC results: parameters & summary statistics (over time)
plot_abc_results <- function(ABC_out,project_dir,bool_pdf=TRUE){
   
   # open pdf stream
   if(bool_pdf) .rstride$create_pdf(project_dir = project_dir,file_name = 'results_ABC')
   
   ## model parameters ----
   par(mfrow=c(3,3))
   # parameters
   for(i in 1:ncol(ABC_out$param)){
      
      hist(ABC_out$param[,i],20,
           xlim = as.numeric(stride_prior[[i]][-1]),
           xlab = names(stride_prior)[i],
           main = names(stride_prior)[i])
      legend('topright',
             title='mean',
             paste(round(mean(ABC_out$param[,i]),digits=2)),
             cex=0.5)
   }
   
   
   for(i in grep('compliance_delay',names(stride_prior))){
      hist(round(ABC_out$param[,i]),
           xlab = names(stride_prior)[i],
           main = paste(names(stride_prior)[i],'\n[DISCRETE]'))
   }
   
   
   ## output statistics ----
   par(mfrow=c(3,3))
   
   ## if  over time
   
   # get categories
   output_cat <- as.character(unique(sum_stat_obs$category))
   
   # iterate over categories
   for(i_cat in output_cat){
      flag_out <- sum_stat_obs$category == i_cat & sum_stat_obs$bool_orig == TRUE
      sum(flag_out)
      dim(sum_stat_obs)
      dim(ABC_out$stats)
      
      # if over time
      if(sum(flag_out)>1){
         y_lim <- range(pretty(c(sum_stat_obs$value[flag_out],
                                 sum_stat_obs$value_low[flag_out],
                                 sum_stat_obs$value_high[flag_out],
                                 ABC_out$stats[,flag_out])),
                        na.rm=T)
         
         plot(sum_stat_obs$date[flag_out],
              sum_stat_obs$value[flag_out],
              ylim = y_lim,
              main = i_cat,
              ylab = i_cat)
         
         if(all(!is.na(sum_stat_obs$value_low[flag_out]))){
            add_interval(x = sum_stat_obs$date[flag_out],
                         y1 = sum_stat_obs$value_low[flag_out],
                         y2 = sum_stat_obs$value_high[flag_out])
         }
         
         for(i_out in 1:nrow(ABC_out$stats)){
            lines(sum_stat_obs$date[flag_out],
                  ABC_out$stats[i_out,flag_out],
                  col=alpha(4,0.8))
         }
      } else{ # else: hist + reference
         
         # initial doubling time
         x_lim <- range(0,pretty(sum_stat_obs$value_high[flag_out]*1.1))
         hist(ABC_out$stats[,flag_out],40,
              xlim=x_lim,
              xlab=i_cat,
              main=i_cat,
              col=alpha(4,0.2),
              border = alpha(4,0.2))
      #   abline(v=mean(sum_stat_obs$value[flag_out]),col=1)
         add_interval_hor(x1  = sum_stat_obs$value_low[flag_out],
                          x2  = sum_stat_obs$value_high[flag_out],
                          x_mean = sum_stat_obs$value[flag_out],
                          col = 1)
      }
      
      
      
   }
   
   # close pdf stream
   if(bool_pdf) dev.off()
}


plot_abc_intermediate <- function(ABC_out,project_dir){
   
   # if intermediate results present ==>> plot
   if( 'intermediary' %in% names(ABC_out)){
   
      # open pdf stream
      .rstride$create_pdf(project_dir = project_dir,file_name = 'results_ABC_intermediate')
      
      for(i_seq in 1:length(ABC_out$intermediary)){
         par(mfrow=c(3,2))
         ABC_out_temp <- ABC_out
         num_param <- length(stride_prior)
         ABC_out_temp$param <- ABC_out$intermediary[[i_seq]]$posterior[,2:(num_param+1)]
         ABC_out_temp$stats <- ABC_out$intermediary[[i_seq]]$posterior[,-(0:(num_param+1))]
         plot_abc_results(ABC_out_temp,project_dir,bool_pdf = FALSE)
      }
      
      dev.off()
      
      
      ## check progress
      .rstride$create_pdf(project_dir = project_dir,file_name = 'results_ABC_posterior')
      get_stat <- function(x){
         c(min(x),mean(x),max(x))
      }
      
      foreach(i = 1:length(ABC_out$intermediary),
              .combine = 'rbind') %do% {
                 
                 c(iter = i,
                   n_simul_tot = ABC_out$intermediary[[i]]$n_simul_tot,
                   tol_step = ABC_out$intermediary[[i]]$tol_step,
                   param1_ = get_stat(ABC_out$intermediary[[i]]$posterior[,2]),
                   param2_ = get_stat(ABC_out$intermediary[[i]]$posterior[,3]),
                   param3_ = get_stat(ABC_out$intermediary[[i]]$posterior[,4]),
                   param4_ = get_stat(ABC_out$intermediary[[i]]$posterior[,5]),
                   param5_ = get_stat(ABC_out$intermediary[[i]]$posterior[,6]),
                   param6_ = get_stat(ABC_out$intermediary[[i]]$posterior[,7]),
                   param7_ = get_stat(ABC_out$intermediary[[i]]$posterior[,8])
                 )
                 
              } -> db_abc
      
      
      par(mfrow=c(3,3))
      db_abc <- data.frame(db_abc)
      plot(db_abc$iter,(db_abc$n_simul_tot),main='num simulations')
      plot(db_abc$iter,log(db_abc$tol_step),main='log(tolerance)')
      
      
      for(i in 1:length(stride_prior)){
         tmp_out <- db_abc[,paste0('param',i,'_',1:3)]
         plot(db_abc$iter,tmp_out[,2],type='l',
              ylim=range(tmp_out),main=names(stride_prior)[i],ylab=names(stride_prior)[i])
         lines(db_abc$iter,tmp_out[,1],lty=2)
         lines(db_abc$iter,tmp_out[,3],lty=2)
      }
      
      # close pdf stream
      dev.off()
   }
}

plot_abc_correlation <- function(ABC_out,project_dir){
   .rstride$create_pdf(project_dir = project_dir,file_name = 'results_ABC_correlation')
   posterior_param <- ABC_out$param
   colnames(posterior_param) <- names(stride_prior)
   corrplot(cor(posterior_param))
   dev.off()
}


################################################ #
## REFERENCE DATA  ----
################################################ #

get_abc_reference_data <- function(ref_period ,
                                   bool_age  = FALSE,
                                   bool_doubling_time = TRUE){
   
   ## hospital reference data ----
   # use (local version of) most recent SCIENSANO data (or backup version)
   hosp_ref_data          <- get_hospital_incidence_age(config_exp$hospital_category_age)
   hosp_ref_data$sim_date <- as.Date(hosp_ref_data$sim_date)
   dim(hosp_ref_data)
   hosp_ref_data          <- hosp_ref_data[hosp_ref_data$sim_date %in% ref_period,]
   
   if(bool_age){
      abc_age_cat       <- seq(0,80,10) #TODO: make generic
      abc_hosp_stat     <- data.table(value      = unlist(hosp_ref_data[,grepl('hospital_admissions_',names(hosp_ref_data))]),
                                      value_low  = NA,
                                      value_high = NA,
                                      date       = rep(hosp_ref_data$sim_date,length(abc_age_cat)),
                                      age_min    = rep(abc_age_cat,each=nrow(hosp_ref_data)),
                                      category   = rep(paste0('new_hospital_admissions_age',1:length(abc_age_cat)),each=length(hosp_ref_data$sim_date)),
                                      bool_orig  = TRUE)
   } else{
      abc_hosp_stat     <- data.table(value      = hosp_ref_data$hospital_admissions,
                                      value_low  = NA,
                                      value_high = NA,
                                      date       = hosp_ref_data$sim_date,
                                      age_min    = NA,
                                      category   = 'new_hospital_admissions',
                                      bool_orig  = TRUE)
   }   
      
   dim(abc_hosp_stat)
   
   ## seroprevalence data ----
   prevalence_ref <- load_observed_seroprevalence_data(ref_period = ref_period,
                                                       analysis = ifelse(bool_age,'age','overall'))
   # temporary fix for 80-90 year olds
   prevalence_ref <- prevalence_ref[prevalence_ref$age_min!=90,]
   
   abc_sero_stat     <- data.table(value      = prevalence_ref$point_incidence_mean,
                                   value_low  = prevalence_ref$point_incidence_low,
                                   value_high = prevalence_ref$point_incidence_high,
                                   date       = prevalence_ref$seroprevalence_date,
                                   age_min    = prevalence_ref$age_min,
                                   category   = 'cumulative_infections',
                                   bool_orig  = TRUE)
   if(bool_age){
      abc_sero_stat[,category := paste0('cumulative_infections_age', as.numeric(prevalence_ref$level))]
   }
   
   ## doubling time 3.1 (2.4-4.4) \cite{pellis2020challenges} ----
   ref_doubling_time <- data.frame(dates    = seq(as.Date('2020-02-24'),as.Date('2020-03-08'),1),
                                   mean     = 3.1,
                                   CI_low   = 2.4,
                                   CI_upper = 4.4
   )
   ref_doubling_time <- ref_doubling_time[ref_doubling_time$dates %in% ref_period,]
   
   if(bool_doubling_time && nrow(ref_doubling_time) > 0){
      abc_dtime_stat    <- data.table(value      = ref_doubling_time$mean,
                                      value_low  = ref_doubling_time$CI_low,
                                      value_high = ref_doubling_time$CI_upper,
                                      date       = ref_doubling_time$dates,
                                      age_min    = NA,
                                      category   = 'doubling_time_march',
                                      bool_orig  = c(TRUE,rep(FALSE,length(ref_doubling_time$mean)-1)))
   } else{
      abc_dtime_stat <- NULL
   }
      
   # combine ----
   sum_stat_obs      <- rbind(abc_hosp_stat,
                              abc_sero_stat,
                              abc_dtime_stat)
   dim(sum_stat_obs)
   
   sero_rep_factor   <- floor(nrow(abc_hosp_stat) / nrow(abc_sero_stat))
   for(i in 2:sero_rep_factor){
      sum_stat_obs <- rbind(sum_stat_obs,abc_sero_stat[, bool_orig := FALSE])
   }
   dim(sum_stat_obs); table(sum_stat_obs$bool_orig)
   
   if(!is.null(abc_dtime_stat)){
      sero_dtime_factor   <- floor(nrow(abc_hosp_stat) / nrow(abc_dtime_stat))
      for(i in 2:sero_dtime_factor){
         sum_stat_obs <- rbind(sum_stat_obs,abc_dtime_stat[,bool_orig := FALSE])
      }
      dim(sum_stat_obs); table(sum_stat_obs$bool_orig)
   }
   
   return(sum_stat_obs)
   
   
   
   nrow(abc_hosp_stat) / nrow(abc_sero_stat)
   
   sero_rep_factor   <- floor(nrow(abc_hosp_stat) / nrow(abc_sero_stat))
   sum_stat_obs      <- rbind(abc_hosp_stat,abc_sero_stat)
   sum_stat_obs['bool_orig'] <- TRUE
   for(i in 2:sero_rep_factor){
      sum_stat_obs <- rbind(sum_stat_obs,
                            cbind(abc_sero_stat,
                                  bool_orig = FALSE))
   }
   
   return(sum_stat_obs)
   
}

