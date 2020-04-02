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

inspect_incidence_data <- function(project_dir,save_pdf = TRUE)
{
  
  # command line message
  smd_print('INSPECT INCIDENCE DATA...')
  
  # load project summary
  project_summary      <- .rstride$load_project_summary(project_dir)
  
  # retrieve all variable model parameters
  input_opt_design     <- .rstride$get_variable_model_param(project_summary)
  
  # get all transmission output
  data_incidence_all      <- .rstride$load_aggregated_output(project_dir,'data_incidence')
  
  if(length(data_incidence_all) == 1 && is.na(data_incidence_all)){
    smd_print('NO INCIDENCE DATA AVAILABLE.')
    return(NA)
  }
  
  ## REFERENCE DATA COVID-19: new hospitalisation
  file_name <- './data/COVID-19-BE-v2.xlsx'
  if(file.exists(file_name))
  {
   burden_of_disaese <- read.xlsx(file_name,detectDates = T)
   burden_of_disaese <- burden_of_disaese[!is.na(burden_of_disaese$Hosp.new),]
   hosp_cases_num    <- burden_of_disaese$Hosp.new
   hosp_cases_cum    <- cumsum(hosp_cases_num)
   hosp_cases_date   <- burden_of_disaese$DateCase
  } else{
    hosp_cases_cum    <- 0
    hosp_cases_date   <- Sys.Date()
  }
  
  
  # open pdf stream
  if(save_pdf) .rstride$create_pdf(project_dir,'incidence_inspection',9,8)
  
  # reset figure arrangements... and start new plot
  if(save_pdf) par(mfrow=c(2,2))
  
  # change figure margins
  par(mar=c(5,5,5,5))
  
  i_config <- 1
  for(i_config in 1:nrow(input_opt_design)){
    
    # subset transmission output corresponding the 'input_opt_design' row
    flag_exp            <- .rstride$get_equal_rows(project_summary,input_opt_design[i_config,])
    data_incidence      <- data_incidence_all[data_incidence_all$exp_id %in% project_summary$exp_id[flag_exp],]
    
    # get population size
    pop_size <- unique(project_summary$population_size[flag_exp])
    
    # get date info
    sim_date_all <- data_incidence$sim_date[data_incidence$exp_id == data_incidence$exp_id[1]]
    
    # get isolation info
    calendar_file <- unique(project_summary$holidays_file[flag_exp])
    opt_calendar <- 'none'
    opt_calendar <- ifelse(grepl('march',calendar_file),'untill April 5th',opt_calendar)
    opt_calendar <- ifelse(grepl('may',calendar_file),'untill May 31th',opt_calendar)
    opt_calendar <- ifelse(grepl('april',calendar_file),'untill April 30th',opt_calendar)
    
    # get other run info: population, calendar, distancing measures
    sel_run_id <- which(flag_exp)[1]
    run_info <- c(paste0('pop_size = ',project_summary$population_size[sel_run_id]/1e3,'k'),
                  paste0('distancing = ',opt_calendar)
    )
    
    if(opt_calendar != 'none'){
      run_info <- c(run_info,
                    paste0('telework_prob = ',project_summary$telework_probability[sel_run_id]),
                    paste0('cnt_reduction_work = ',project_summary$cnt_reduction_work[sel_run_id]),
                    paste0('cnt_reduction_other = ',project_summary$cnt_reduction_other[sel_run_id])
      )
    }
    
    
    # remove holiday file from legend
    input_no_holidays_file <- !grepl('holidays_file',colnames(input_opt_design))
    if(sum(input_no_holidays_file) > 0 && nrow(input_opt_design)>0){ # add some param info to the legend
      run_info <- c(run_info,paste0(colnames(input_opt_design)[input_no_holidays_file],': ',input_opt_design[i_config,input_no_holidays_file]))
    }

    ## HELP FUNCTION
    get_incidence_statistics <- function(case_type){
    
      aggr_formula <- formula(paste(case_type,'~ sim_date'))  
      cases_mean   <- aggregate(aggr_formula,data=data_incidence,mean)
      cases_970p   <- aggregate(aggr_formula,data=data_incidence,quantile,0.975)
      cases_025p   <- aggregate(aggr_formula,data=data_incidence,quantile,0.025)
      
      out <- data.frame(cases_mean,cases_970p[,2],cases_025p[,2])
      names(out) <- c('sim_date','cases_mean','cases_975p','cases_025p')
      
      return(out)
    }
    
    ## INFECTIONS
    new_infections <- get_incidence_statistics('new_infections')
    
    ## INFECTIOUS
    new_infectious_cases <- get_incidence_statistics('new_infectious_cases')
    
    ## SYMPTOMATIC
    new_symptomatic_cases <- new_symptomatic_cases <- get_incidence_statistics('new_symptomatic_cases')
    
    ## HOSPITAL PROXY: FRACTION SYMPTOMATIC AND DELAY
    hosp_fraction  <- 0.2
    hosp_delay     <- 7 #days
    data_incidence$new_hospital_cases <- data_incidence$new_symptomatic_cases * hosp_fraction
    new_hospital_cases <- get_incidence_statistics('new_hospital_cases')
    new_hospital_cases$sim_date <- new_hospital_cases$sim_date + hosp_delay
    
    # BE POP SIZE
    pop_size_be <- 11e6
    
    plot_ylim <- c(0,max(data_incidence_all[,grepl('new_',names(data_incidence_all))]*1.4,na.rm=T))
    
    for(sel_ylim in list(plot_ylim,c(0,2000))){
      
      plot_main <- paste('Social distancing:',opt_calendar)
      bool_legend <- TRUE
      if(any(sel_ylim != plot_ylim)){
        plot_main   <- paste(plot_main,'\n[zoom]')
        bool_legend <- FALSE
      }
      
      flag_plot <- new_infections$sim_date > as.Date('2020-03-01')
      plot(new_infections$sim_date[flag_plot],new_infections$cases_mean[flag_plot],type='l',
           xlab='Time',ylab='New cases',col=1,lwd=3,
           ylim=sel_ylim,
           main=plot_main)
      grid(nx=NA,ny=NULL,lty=3,col='lightgray')
      abline(v=sim_date_all,lty=3,col='lightgray')
      lines(new_infections$sim_date,new_infections$cases_mean,col=1,lwd=3)
      lines(new_infectious_cases$sim_date,new_infectious_cases$cases_mean,col=2,lwd=3)
      lines(new_symptomatic_cases$sim_date,new_symptomatic_cases$cases_mean,col=4,lwd=3)
      
      # hospital proxy
      lines(new_hospital_cases$sim_date,new_hospital_cases$cases_mean,col=4,lwd=3,lty=2)
      
      ## ADD HOSPITAL CASES FROM BELGIUM
      points(hosp_cases_date,hosp_cases_num,pch=15,col=8)
      
      abline(v=sim_date_all[sim_date_all=='2020-03-14'])
      #abline(v=sim_date_all[sim_date_all=='2020-04-05'])
      abline(v=Sys.Date())
      
      # add axis on right-hand side with population/Belgian perspective
      y_ticks  <- pretty(sel_ylim)
      if(all(sel_ylim == plot_ylim)){
        y_labels <- format(y_ticks/pop_size*100,scientific = F,digits=1)
        mtext('simulated population (%)',side = 4,line=3,cex=0.8)
      } else{
        y_labels <- format(y_ticks/pop_size_be*100,scientific = F,digits=1)
        mtext('Belgian population (%)',side = 4,line=3,cex=0.8)
      }
      axis(4,y_ticks,y_labels,las=2)
      
      # add legend
      
        if(bool_legend)
        {
          legend('topleft',
                 c('infections (mean)',
                   'infectious (mean)',
                   'symptomatic (mean)',
                   paste0(hosp_fraction*100,'% symptomatic +',hosp_delay,'days'),
                   'hospital cases (BE)'),
                 col=c(1,2,4,4,8),
                 lty=c(1,1,1,3,NA),
                 pch=c(NA,NA,NA,NA,15),
                 bg='white',
                 lwd=2,
                 cex=0.7
          )
          
          legend('topright',
               run_info,
               bg='white',
               cex=0.6
        )
      }
    }
   
  
    # CUMMULATIVE
    plot_ylim <- range(cumsum(new_infections$cases_mean))
    for(sel_ylim in list(plot_ylim,c(0,max(hosp_cases_cum)*1.2))){
      
      sel_x_values <- range(hosp_cases_date-7,hosp_cases_date+7)
      if(all(sel_ylim == plot_ylim)){
        sel_x_values <- range(new_infections$sim_date)
      }
      sel_xlim     <- range(sel_x_values)
      
      plot_main <- gsub('zoom','cumulative',plot_main)
      plot(new_infections$sim_date,cumsum(new_infections$cases_mean),type='l',
           xlab='Time',
           ylab='cumulative cases',
           col=0,lwd=3,
           ylim=sel_ylim,
           xlim=range(sel_x_values),
           main=plot_main,
           xaxt='n')
      axis(1,pretty(sel_x_values),pretty(sel_x_values))
      grid(nx=NA,ny=NULL,lty=3,col='lightgray')
      abline(v=sim_date_all,lty=3,col='lightgray')
      abline(v=sim_date_all[sim_date_all=='2020-03-14'])
      #abline(v=sim_date_all[sim_date_all=='2020-04-05'])
      abline(v=Sys.Date())
      
      y_ticks  <- pretty(sel_ylim)
      if(all(sel_ylim == plot_ylim)){
        y_labels <- format(y_ticks/pop_size*100,scientific = F,digits=1)
        mtext('simulated population (%)',side = 4,line=3,cex=0.8)
      } else{
        y_labels <- format(y_ticks/pop_size_be*100,scientific = F,digits=1)
        mtext('Belgian population (%)',side = 4,line=3,cex=0.8)
      }
      axis(4,y_ticks,y_labels,las=2)
      
      lines(new_infections$sim_date,cumsum(new_infections$cases_mean),col=1,lwd=3)
      lines(new_infectious_cases$sim_date,cumsum(new_infectious_cases$cases_mean),col=2,lwd=3)
      lines(new_symptomatic_cases$sim_date,cumsum(new_symptomatic_cases$cases_mean),col=4,lwd=3)
    
      ## add hospital proxy
      lines(new_hospital_cases$sim_date,cumsum(new_hospital_cases$cases_mean),col=4,lwd=3,lty=2)
      
      ## ADD HOSPITAL CASES FROM BELGIUM
      points(hosp_cases_date,hosp_cases_cum,pch=15,col=8)
      
      if(all(sel_ylim  == plot_ylim)){
        legend('topleft',
               c(paste0(hosp_fraction*100,'% symptomatic +',hosp_delay,'days'),
                 'hospital cases (BE)'),
               col=c(4,8),
               pch=c(NA,15),
               bg='white',
               lty=c(3,0),
               lwd=c(2,0),
               cex=0.7
        )
      }
      
      #}
    } # end for-loop: cumulative plot
    
  } # end for-loop to vary the input_opt_design
  
  # close PDF stream
  if(save_pdf) dev.off()
  
  # command line message
  smd_print('INSPECTION OF INCIDENCE DATA COMPLETE')
  
} # function end


## TEMPORARY BACK CALCULATION
if(0==1){
  ## REFERENCE DATA COVID-19: new hospitalisation
  file_name <- './data/COVID-19-BE-v2.xlsx'
  burden_of_disaese <- read.xlsx(file_name,detectDates = T)
  burden_of_disaese <- burden_of_disaese[!is.na(burden_of_disaese$Hosp.new),]
  hosp_cases_num    <- burden_of_disaese$Hosp.new
  hosp_cases_date   <- burden_of_disaese$DateCase
  
  confirmed_cases <- burden_of_disaese$CC.new

  sympt_cases      <- hosp_cases_num * 5    # 20% of symptomatic cases is hospitalized
  sympt_cases_date <- hosp_cases_date - 7   # 7 delay between start symptom onset and hospital admission
  
  infected_cases      <- sympt_cases * 2    # 50% of the cases is symptomatic
  infected_cases_date <- sympt_cases_date - 5 # 5 day delay between infection and symptom onset

  incidence_data <-  merge(merge(data.frame(date = infected_cases_date,infected_cases),
                                 data.frame(date = sympt_cases_date,sympt_cases),all = T),
                           data.frame(date=hosp_cases_date,hosp_cases_num),all=T)
  
  
  plot(incidence_data$date,incidence_data$infected_cases,lwd=2,type='l', xlab='date',ylab='incidence')
  lines(incidence_data$date,incidence_data$sympt_cases,lwd=2,col=2)
  lines(incidence_data$date,incidence_data$hosp_cases_num,col=4,lwd=2)
  lines(hosp_cases_date,confirmed_cases,col=5,lwd=2)
  grid()  
  legend('topleft',
         c('total infections',
           'symptomatic cases',
           'hospitalisations',
           'confirmed cases'),
         lwd=2,
         col=c(1,2,4,5),
         cex=0.7)    

  
  
  
  
  }

  

