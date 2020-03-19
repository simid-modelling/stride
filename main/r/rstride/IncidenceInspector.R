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

inspect_incidence_data <- function(project_dir)
{
  
  # command line message
  smd_print('INSPECT INCIDENCE DATA...')
  
  # load project summary
  project_summary      <- .rstride$load_project_summary(project_dir)
  
  # retrieve all variable model parameters
  input_opt_design     <- .rstride$get_variable_model_param(project_summary)
  
  # get all transmission output
  data_incidence_all      <- .rstride$load_aggregated_output(project_dir,'data_incidence')
  
  plot_ylim <- c(0,max(data_incidence_all[,grepl('new_',names(data_incidence_all))],na.rm=T))
  
  if(length(data_incidence_all) == 1 && is.na(data_incidence_all)){
    smd_print('NO INCIDENCE DATA AVAILABLE.')
    return(NA)
  }
  
  # open pdf stream
  .rstride$create_pdf(project_dir,'incidence_inspection',10,7)
  
  # reset figure arrangements... and start new plot
  par(mfrow=c(2,2))
  
  i_config <- 1
  for(i_config in 1:nrow(input_opt_design)){
    
    # subset transmission output corresponding the 'input_opt_design' row
    flag_exp            <- .rstride$get_equal_rows(project_summary,input_opt_design[i_config,])
    data_incidence      <- data_incidence_all[data_incidence_all$exp_id %in% project_summary$exp_id[flag_exp],]
    
    # get date info
    sim_date_all <- data_incidence$sim_date[data_incidence$exp_id == data_incidence$exp_id[1]]
    
    # get isolation info
    calendar_file <- unique(project_summary$holidays_file[flag_exp])
    opt_calendar <- 'none'
    opt_calendar <- ifelse(grepl('calendar',calendar_file),'untill April 3th',opt_calendar)
    opt_calendar <- ifelse(grepl('extended',calendar_file),'untill May 31th',opt_calendar)
    opt_calendar <- ifelse(grepl('april',calendar_file),'untill April 30th',opt_calendar)
    
    # get other run info: population, calendar, distancing measures
    sel_run_id <- which(flag_exp)[1]
    run_info <- c(paste0('pop_size = ',project_summary$population_size[sel_run_id]),
                  paste0('distancing = ',opt_calendar),
                  paste0('telework_prob = ',project_summary$telework_probability[sel_run_id]),
                  paste0('cnt_reduction_work = ',project_summary$cnt_reduction_work[sel_run_id]),
                  paste0('cnt_reduction_other = ',project_summary$cnt_reduction_other[sel_run_id])
    )
    
    
    ## HELP FUNCTION
    get_incidence_statistics <- function(case_type,plot_tag){
    
      aggr_formula <- formula(paste(case_type,'~ sim_date'))  
      cases_mean   <- aggregate(aggr_formula,data=data_incidence,mean)
      cases_970p   <- aggregate(aggr_formula,data=data_incidence,quantile,0.975)
      cases_025p   <- aggregate(aggr_formula,data=data_incidence,quantile,0.025)
      
      out <- data.frame(cases_mean,cases_970p[,2],cases_025p[,2])
      names(out) <- c('sim_date','cases_mean','cases_975p','cases_025p')
      
      return(out)
    }
    
    ## INFECTIONS
    new_infections <- get_incidence_statistics('new_infections','new infections')
    
    ## INFECTIOUS
    new_infectious_cases <- get_incidence_statistics('new_infectious_cases','new infectious cases')
    
    ## SYMPTOMATIC
    new_symptomatic_cases <- new_symptomatic_cases <- get_incidence_statistics('new_symptomatic_cases','new symptomatic cases')
    
    for(sel_ylim in list(plot_ylim,c(0,4000))){
      
      plot_main <- paste('Social distancing:',opt_calendar)
      if(any(sel_ylim != plot_ylim)){
        plot_main <- paste(plot_main,'\n[zoom]')
      }
      
      plot(new_infections$sim_date,new_infections$cases_mean,type='l',
           xlab='Time',ylab='cases',col=1,lwd=3,
           ylim=sel_ylim,
           main=plot_main)
      grid(nx=NA,ny=NULL,lty=3,col='lightgray')
      abline(v=sim_date_all,lty=3,col='lightgray')
      lines(new_infections$sim_date,new_infections$cases_mean,col=1,lwd=3)
      lines(new_infectious_cases$sim_date,new_infectious_cases$cases_mean,col=2,lwd=3)
      lines(new_symptomatic_cases$sim_date,new_symptomatic_cases$cases_mean,col=4,lwd=3)
      
      abline(v=sim_date_all[sim_date_all=='2020-03-14'])
      abline(v=sim_date_all[sim_date_all=='2020-04-03'])
      
      legend('topleft',
             c('infections (mean)',
               'infectious (mean)',
               'symptomatic (mean)'),
             col=c(1,2,4),
             bg='white',
             lwd=2,
             cex=0.7
      )
      
      legend('left',
             run_info,
             bg='white',
             cex=0.5
      )
      
      
    }
   
      
  } # end for-loop to vary the input_opt_design
  
  # close PDF stream
  dev.off()
  
  # command line message
  smd_print('INSPECTION OF INCIDENCE DATA COMPLETE')
  
} # function end

