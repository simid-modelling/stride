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
# MODEL INCIDENCE EXPLORATION
#
#############################################################################

#' @param project_dir   name of the project folder
#' @param num_selection the number of experiments with minimal LS score to select and present
inspect_incidence_data <- function(project_dir, num_selection = 4, bool_add_param=TRUE)
{
  
  #debug
  if(!exists('num_selection')) {num_selection = 4}
  if(!exists('bool_add_param')) {bool_add_param = TRUE}
  
  # load project summary
  project_summary    <- .rstride$load_project_summary(project_dir)
  
  # retrieve all variable model parameters
  input_opt_design   <- .rstride$get_variable_model_param(project_summary)
  
  # get all transmission output
  data_incidence_all <- .rstride$load_aggregated_output(project_dir,'data_incidence')
  
  if(length(data_incidence_all) == 1 && is.na(data_incidence_all)){
    smd_print('NO INCIDENCE DATA AVAILABLE.')
    return(NA)
  }
  
 
  # add config_id 
  # get variable names of input_opt_design (fix if only one column)
  if(ncol(input_opt_design) == 1) {
    project_summary$config_id  <- project_summary[,colnames(input_opt_design)]
    input_opt_design           <- data.frame(input_opt_design,config_id = c(input_opt_design))
  } else{
    project_summary$config_id  <- apply(project_summary[,names(input_opt_design)],1,paste, collapse='_')
    input_opt_design$config_id <- apply(input_opt_design,1,paste, collapse='_')
  }
  
  # add config_id to incidence data
  data_incidence_all         <- merge(data_incidence_all,project_summary[,c('exp_id','config_id')] )

  
  ## REFERENCE DATA COVID-19: new hospitalisation
  file_name <- './data/covid19.csv'
  burden_of_disaese  <- read.table(file_name,sep=',',header=T,stringsAsFactors = F)
  hosp_cases_num     <- burden_of_disaese$Sum.of.NewPatientsNotReferred + burden_of_disaese$Sum.of.NewPatientsReferred 
  hosp_cases_cum     <- cumsum(hosp_cases_num)
  hosp_cases_date    <- as.Date(burden_of_disaese$DateCase)
  
  # aggregate into one data.frame
  hosp_adm_data <- data.frame(date    = hosp_cases_date,
                              num_adm = hosp_cases_num,
                              cum_adm = hosp_cases_cum)
  
  # remove reference data if simulation period is shorter
  flag_compare  <- hosp_adm_data$date %in% data_incidence_all$sim_date
  hosp_adm_data <- hosp_adm_data[flag_compare,]
  
  # create columns for hosptical cases and least square score
  data_incidence_all$cummulative_hospital_cases <- NA
  data_incidence_all$ls_score_hosp              <- NA
  
  # create matrix to reformat hospital data
  hosp_data_ls     <- matrix(NA, ncol = 1, nrow = nrow(project_summary))
  hosp_data_tag    <- hosp_data_ls
  
  i_exp <- 1  
  # loop over each experiment
  for(i_exp in unique(data_incidence_all$exp_id)){
    
    # select one run
    flag_exp       <- data_incidence_all$exp_id == i_exp
    
    # calculate cummulative cases
    data_incidence_all$cummulative_infections[flag_exp]        <- cumsum(data_incidence_all$new_infections[flag_exp])
    data_incidence_all$cummulative_infectious_cases[flag_exp]  <- cumsum(data_incidence_all$new_infectious_cases[flag_exp])
    data_incidence_all$cummulative_symptomatic_cases[flag_exp] <- cumsum(data_incidence_all$new_symptomatic_cases[flag_exp])
    data_incidence_all$cummulative_hospital_cases[flag_exp]    <- cumsum(data_incidence_all$new_hospital_admissions[flag_exp])
    
    # age specific hospital admissions
    data_incidence_all$cummulative_hospital_cases_age1[flag_exp]    <- cumsum(data_incidence_all$new_hospital_admissions_age1[flag_exp])
    data_incidence_all$cummulative_hospital_cases_age2[flag_exp]    <- cumsum(data_incidence_all$new_hospital_admissions_age2[flag_exp])
    data_incidence_all$cummulative_hospital_cases_age3[flag_exp]    <- cumsum(data_incidence_all$new_hospital_admissions_age3[flag_exp])
    data_incidence_all$cummulative_hospital_cases_age4[flag_exp]    <- cumsum(data_incidence_all$new_hospital_admissions_age4[flag_exp])
    
    # Sum of Squares: score
    flag_hosp_data       <- flag_exp & data_incidence_all$sim_date %in% hosp_adm_data$date
    ls_score_hosp        <- sum(sqrt((data_incidence_all$new_hospital_admissions[flag_hosp_data] - hosp_adm_data$num_adm)^2))
    hosp_data_ls[i_exp,] <- sum(ls_score_hosp)
    hosp_data_tag[i_exp,]<- unique(data_incidence_all$config_id[flag_exp])
  }
  
  # get mean ls per config id
  ls_summary_config        <- aggregate(hosp_data_ls,list(hosp_data_tag) , mean, na.rm=T)
  names(ls_summary_config) <- c('config_tag','ls_score_hosp')
  
  # select the 'num_selection' best parameter sets
  num_selection  <- min(num_selection,length(ls_summary_config$ls_score_hosp))
  ls_order       <- order(ls_summary_config$ls_score_hosp,decreasing = F)
  config_tag_sel <- ls_summary_config[ls_order[1:num_selection],]
  
  # select incidence data
  flag_plot               <- data_incidence_all$config_id %in% config_tag_sel$config_tag
  data_incidence_ensemble <- data_incidence_all[flag_plot,]
  
  # open pdf stream (ENSEMBLE)
  .rstride$create_pdf(project_dir,'incidence_ensemble',width = 6, height = 7)
  par(mfrow=c(4,1))
  
  # plot temporal patterns (ensemble)
  plot_incidence_data(data_incidence_ensemble,project_summary,
                      hosp_adm_data,input_opt_design,bool_add_param
  )
  
  # close pdf stream
  dev.off()
  
  ## PER CONFIG: plot temporal patterns
  .rstride$create_pdf(project_dir,'incidence_inspection',width = 6, height = 8)
  par(mfrow=c(4,1))
  
  opt_config_id <- unique(data_incidence_all$config_id)
  i_config <- opt_config_id[1]
  for(i_config in opt_config_id){
    
    # select subset
    data_incidence_sel <- data_incidence_all[data_incidence_all$config_id == i_config,]
    
    # plot
    plot_incidence_data(data_incidence_sel,project_summary,
                        hosp_adm_data,input_opt_design,
                        bool_add_param)
  }
  
  # close pdf
  dev.off()
  #--------------------------#
  
  ## PER R0: plot temporal patterns
  .rstride$create_pdf(project_dir,'incidence_R0',width = 6, height = 8)
  par(mfrow=c(4,1))
  
  opt_r0 <- unique(input_opt_design$r0)
  i_r0 <- opt_r0[1]
  for(i_r0 in opt_r0){
    
    # select config_id
    opt_config_id <- unique(input_opt_design$config_id[input_opt_design$r0 ==  i_r0])
    
    # select subset
    data_incidence_sel <- data_incidence_all[data_incidence_all$config_id %in% opt_config_id,]
    dim(data_incidence_sel)
    
    # plot
    plot_incidence_data(data_incidence_sel,project_summary,
                        hosp_adm_data,input_opt_design,
                        bool_add_param)
  }
  
  # close pdf
  dev.off()
  
  # ## AVERAGE
  # num_runs <- unique(table(project_summary$config_id))
  # tmp <- aggregate( new_hospital_admissions ~ sim_day + sim_date + config_id, data = data_incidence_sel, mean)
  # tmp$new_hospital_admissions[tmp$sim_day == min(tmp$sim_day)] <- NA
  # plot(tmp$sim_date,tmp$new_hospital_admissions,
  #      type='l',
  #      lwd=3,
  #      col=pcolor$H,
  #      ylab='New hospital admissions',
  #      xlab='Time',
  #      main = paste('Average of ', num_runs, 'realisations'))
  # grid()
  # points(hosp_cases_date,hosp_cases_num,col=1,pch=16)
  # add_breakpoints()
  # add_legend_hosp()
  
  #--------------------------#
  # parameters
  
  if(nrow(input_opt_design)>1){
    flag_param   <- input_opt_design$config_id %in% config_tag_sel$config_tag
    print(input_opt_design[flag_param,-ncol(input_opt_design)])
    
    # flag_param <- project_summary$config_id %in% config_tag_sel$config_tag
    # project_summary[flag_param,]
  }
  
  
} # end function

plot_incidence_data <- function(data_incidence_sel,project_summary,
                                hosp_adm_data,input_opt_design,bool_add_param){

  # change figure margins
  par(mar=c(5,5,1,5))
  
  # set color definitions and other layout definitions
  pcolor <- data.frame(E = "black",  # exposed (or total infections)
                       I = "darkgoldenrod3",  # infectious
                       S = "red",  # symptomatic
                       H = 'blue',  # hospitalized
                       D = 'black',
                       alpha = 0.1,
                       lwd = 3,
                       stringsAsFactors = F)  # data
  
  # change transparancey for low number of rng-runs
  if(max(table(project_summary$config_id)) < 10){
    pcolor$alpha <- 0.2
  }
  
  # set Belgian population
  pop_size_be <- 11e6
  
  ## FIX FOR PLOTTING: Set all values for the last sim_day to NA
  data_incidence_sel[data_incidence_sel$sim_day %in% max(data_incidence_sel$sim_day,na.rm=T),] <- NA
  
  # set y-lim
  y_lim <- range(0,max(hosp_adm_data$num_adm)*1.3,na.rm=T)
  
  ## HOSPITAL ADMISSIONS ####
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$new_hospital_admissions,
       type='l',
       col=alpha(pcolor$H,pcolor$alpha),
       ylab='New cases',
       xlab='Time',
       ylim = y_lim)
  grid()
  points(hosp_adm_data$date,hosp_adm_data$num_adm,col=pcolor$D,pch=16)
  add_breakpoints()
  add_legend_hosp(pcolor)
  
  ## CUMMULATIVE: HOSPITAL ####
  y_lim <- range(0,max(hosp_adm_data$cum_adm)*1.3,na.rm=T)
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$cummulative_hospital_cases,
       type='l',
       col=alpha(pcolor$H,pcolor$alpha),
       ylab='Total cases (x1000)',
       xlab='Time',
       yaxt='n',
       ylim= range(y_lim))
  axis(2,y_lim,y_lim/1e3)
  axis(4,y_lim,round(y_lim/pop_size_be*100,digits=2))
  mtext('Belgian population (%)',side = 4,line=3,cex=0.7)
  grid()
  points(hosp_adm_data$date,hosp_adm_data$cum_adm,col=pcolor$D,pch=16)
  add_breakpoints()
  add_legend_hosp(pcolor)
  
  lines(data_incidence_sel$sim_date,
       data_incidence_sel$cummulative_hospital_cases_age1,
       col=5)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cummulative_hospital_cases_age2,
        col=6)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cummulative_hospital_cases_age3,
        col=7)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cummulative_hospital_cases_age4,
        col=8)
  
  legend('bottomleft',
         rev(c('0-18',
           '19-59',
           '60-79',
           '+80')),
         col=c(8:5),
         lwd=2,
         title='Age group',
         cex=0.5
  )
  
  ## INCIDENCE: ALL ####
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$new_infections,
       type='l',
       col=alpha(pcolor$E,pcolor$alpha),
       ylab='New cases',
       xlab='Time')
  grid()
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$new_infectious_cases,
        col=alpha(pcolor$I,pcolor$alpha))
  
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$new_symptomatic_cases,
        col=alpha(pcolor$S,pcolor$alpha))
  
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$new_hospital_admissions,
        col=alpha(pcolor$H,pcolor$alpha))
  points(hosp_adm_data$date,hosp_adm_data$num_adm,col=pcolor$D,pch=16)
  add_breakpoints()
  add_legend_all(pcolor)
  
  ## CUMMULATIVE: ALL STATES ####
  y_lim <- pretty(data_incidence_sel$cummulative_infections)
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$cummulative_infections,
       type='l',
       col=alpha(pcolor$E,pcolor$alpha),
       ylab='Total cases (x1000)',
       xlab='Time',
       yaxt='n')
  axis(2,y_lim,y_lim/1e3)
  axis(4,y_lim,round(y_lim/pop_size_be*100,digits=1))
  grid()
  mtext('Belgian population (%)',side = 4,line=3,cex=0.7)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cummulative_infectious_cases,
        col=alpha(pcolor$I,pcolor$alpha))
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cummulative_symptomatic_cases,
        col=alpha(pcolor$S,pcolor$alpha))
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cummulative_hospital_cases,
        col=alpha(pcolor$H,pcolor$alpha))
  points(hosp_adm_data$date,hosp_adm_data$cum_adm,col=pcolor$D,pch=16)
  add_breakpoints()
  if(bool_add_param) {
    add_legend_runinfo(project_summary,input_opt_design,
                       unique(data_incidence_sel$config_id))
  } else {
    add_legend_all(pcolor)
  }
  
  
  
  
} # end function to plot figure

# define the vertical breaks on the plots
add_breakpoints <- function(){
  abline(v=as.Date("2020-03-14"))
  abline(v=Sys.Date())
}

# define the legend for hospital(-only) plots
add_legend_hosp <- function(pcolor){
  legend('topleft',
         c('Hospital admissions (data)',
           '20% symptomatic cases ±6 days'),
         col=c(pcolor$D,pcolor$H),
         pch=c(16,NA),
         lwd=c(NA,2),
         bg='white',
         cex = 0.8)
}

# define the legend with all categories
add_legend_all <- function(pcolor){
  legend('topleft',
         c('New infections',
           'Infectious cases',
           'Symptomatic cases',
           '20% Sympt. cases ±6 days',
           'Hospital admissions (data)'),
         col=unlist(pcolor),
         pch=c(NA,NA,NA,NA,16),
         lwd=c(2,2,2,2,NA),
         cex=0.8,
         bg='white')
}

add_legend_runinfo <- function(project_summary,input_opt_design,
                               i_config){
  
  # remove NA from i_config
  i_config <- i_config[!is.na(i_config)]
  
  # subset project summary
  project_summary_sel <- project_summary[project_summary$config_id %in% i_config,]
  
  # get lockdown info
  calendar_file <- unique(project_summary_sel$holidays_file)
  opt_calendar <- 'none'
  opt_calendar <- ifelse(grepl('march',calendar_file),'untill April 5th',opt_calendar)
  opt_calendar <- ifelse(grepl('may',calendar_file),'untill May 31th',opt_calendar)
  opt_calendar <- ifelse(grepl('april',calendar_file),'untill April 30th',opt_calendar)
  
  # get other run info: population, calendar, distancing measures
  run_info <- c(paste0('pop_size = ',unique(project_summary_sel$population_size)/1e3,'k'),
                paste0('distancing = ',opt_calendar)
  )
  
  if(opt_calendar != 'none'){
    run_info <- c(run_info,
                  paste0('telework_prob = ',paste(unique(project_summary_sel$telework_probability),collapse=', ')),
                  paste0('cnt_reduction_work = ',paste(unique(project_summary_sel$cnt_reduction_work),collapse=", ")),
                  paste0('cnt_reduction_other = ',paste(unique(project_summary_sel$cnt_reduction_other),collapse=", ")),
                  paste0('compliance_delay = ',paste(unique(project_summary_sel$compliance_delay),collapse=", "))
    )
  }
  
  # add other exp_design parameters (excl. holiday file)
  names_exp_param <- colnames(input_opt_design)
  flag_col        <- names_exp_param %in% c('telework_prob','cnt_reduction_work',
                                            'cnt_reduction_other','compliance_delay',
                                            'holidays_file','config_id')
  names_exp_param <- names_exp_param[!flag_col]
  if(length(names_exp_param)>0){
    for(i_name in names_exp_param){
      run_info <- c(run_info,
                    paste0(i_name, ' = ',paste(unique(project_summary_sel[,i_name]),collapse=', '))
                    )
    }
    
  }
  
  # present info in legend
  legend('topleft',
         run_info,
         cex=0.8,
         bg='white')
}


plot_distancing <- function(project_summary){
  
  opt_distancing <- unique(data.frame(compliance_delay = project_summary$compliance_delay,
                                 cnt_reduction_work = project_summary$cnt_reduction_work,
                                 cnt_reduction_other = project_summary$cnt_reduction_other))


  date_start <- as.Date('2020-03-14')
  
  i_distancing <- 1    
  
  opt_distancing[i_distancing,]
  num_days <- opt_distancing$compliance_delay[i_distancing]
  date_all <- date_start + (1:num_days)-1
  compliance_factor <- 1:num_days / num_days
  
  plot(date_all,compliance_factor*opt_distancing$cnt_reduction_work[i_distancing],
       type='l',
       col=4,
       ylim=0:1)
  lines(date_all,compliance_factor*opt_distancing$cnt_reduction_other[i_distancing])
        
    
  
}

