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

############################################################################# #
# EXPLORE TRANSMISSION EVENTS, OUTBREAKS GENERATION INTERVALS AND R0       ####
############################################################################# #
inspect_transmission_data <- function(project_dir,save_pdf = TRUE)
{
  # command line message
  smd_print('INSPECT TRANSMISSION DYNAMICS...')
  
  # load project summary
  project_summary      <- .rstride$load_project_summary(project_dir)
  
  # retrieve all variable model parameters
  input_opt_design     <- .rstride$get_variable_model_param(project_summary)
  
  # get all transmission output
  data_transm_all      <- .rstride$load_aggregated_output(project_dir,'data_transmission')
  
  if(length(data_transm_all) == 1 && is.na(data_transm_all)){
    smd_print('NO TRANSMISSION DATA AVAILABLE.')
    return()
  }
  
  # open pdf stream
  if(save_pdf) .rstride$create_pdf(project_dir,'transmission_inspection',10,7)

  i_config <- 1
  for(i_config in 1:nrow(input_opt_design)){
    
    # reset figure arrangements... and start new plot
    if(save_pdf) par(mfrow=c(3,3))
    
    # subset transmission output corresponding the 'input_opt_design' row
    flag_exp            <- .rstride$get_equal_rows(project_summary,input_opt_design[i_config,])
    data_transm         <- data_transm_all[data_transm_all$exp_id %in% project_summary$exp_id[flag_exp],]
    num_runs_exp        <- sum(flag_exp)
    num_daily_seeds     <- sum(data_transm$sim_day == 1 & is.na(data_transm$infector_id)) / num_runs_exp
    num_infected_seeds  <- sum(data_transm$sim_day==0) / num_runs_exp - num_daily_seeds
  
    
    # get population size
    pop_size            <- unique(project_summary$population_size[flag_exp])
    # pop_size_belgium    <- 11.6e6
    #pop_factor_100k     <- 1/pop_size*1e5
    # pop_factor_belgium  <- 1/pop_size*pop_size_belgium
    
    # get the simulated dates
    # TODO: include checks!
    start_date   <- as.Date(unique(project_summary$start_date[flag_exp]),'%Y-%m-%d')
    num_days     <- unique(project_summary$num_days[flag_exp])
    sim_day_date <- seq(start_date,start_date+num_days,1)
    data_transm$sim_day_date <- start_date + data_transm$sim_day
    
    ## INCIDENCE   ----
    tbl_transm        <- table(data_transm$sim_day,data_transm$exp_id)
    tbl_transm_matrix <- matrix(c(tbl_transm),nrow=nrow(tbl_transm),dimnames=list(rownames(tbl_transm)))
    tbl_transm_date   <- sim_day_date[as.numeric(row.names(tbl_transm_matrix))+1]
    if(nrow(tbl_transm_matrix)==0) tbl_transm_matrix <- matrix(0,nrow=1,ncol=1,dimnames=list(0))
    boxplot(t(tbl_transm_matrix),
            at=tbl_transm_date,
            main=paste0('Incidence: per day\n(total population ',pop_size/1e3,'k)'),
            xlab='time (days)',ylab='New cases',
            xaxt='n')
    axis(1,pretty(tbl_transm_date),format(pretty(tbl_transm_date),"%d %b"))
    grid()
    legend_info <- c(paste('num. runs:',num_runs_exp),
                     paste('inf. seeds / run:',num_infected_seeds),
                     paste('daily inf. seeds:',num_daily_seeds))
    
    if(nrow(input_opt_design)>0){ # add some param info to the legend
      legend_info <- c(legend_info,paste0(colnames(input_opt_design),': ',input_opt_design[i_config,]))
    }
    legend('topleft',legend_info,cex=0.8,bg='white')
    
    abline(v=sim_day_date[sim_day_date == '2020-03-14'])
    abline(v=sim_day_date[sim_day_date == '2020-04-06'])
    
    ## CUMMULATIVE INCIDENCE: AVERAGE     ----
    boxplot(t(apply(tbl_transm_matrix,2,cumsum)),
            at=tbl_transm_date,
            xlab='Date',ylab='Cummulative incidence',
            main=paste0('Incidence: cummulative\n(total population ',pop_size/1e3,'k)'),
            xaxt='n')
    axis(1,pretty(tbl_transm_date),format(pretty(tbl_transm_date),"%d %b"))
    grid()
    
    abline(v=sim_day_date[sim_day_date == '2020-03-14'])
    abline(v=sim_day_date[sim_day_date == Sys.Date()])
    
    ## DOUBLING TIME      ----
    mean_cum_cases        <- cumsum(rowMeans(tbl_transm_matrix))
    mean_cum_cases_double <- mean_cum_cases*2
    mean_doubling_time    <- rep(NA,length(mean_cum_cases))
    i_day <- 50
    for(i_day in 1:length(mean_cum_cases)){
      sel_days <- mean_cum_cases_double[i_day] < mean_cum_cases
      if(any(sel_days)){
        day_double <- min(which(sel_days))
        mean_doubling_time[i_day] <- day_double - i_day
      }
      
    }
    plot(tbl_transm_date,mean_doubling_time,ylim=c(0,14),
         xlab='Date',
         ylab='Doubling time (infections)',
         main='Doubling time (infections)')
    
    abline(v=sim_day_date[sim_day_date == '2020-03-14'])
    abline(v=sim_day_date[sim_day_date == Sys.Date()])
    
    # plot(range(incidence_days),c(0,1.4),col=0,xlab='day',ylab='relative daily incidence',
    #      main='transmission context over time',yaxt='n')
    # axis(2,seq(0,1,0.2),cex.axis=0.9)
    # for(i_loc in 1:ncol(tbl_loc_transm)){
    #   points(incidence_days, tbl_loc_transm[,i_loc]/incidence_day_total,col=i_loc,lwd=4,pch=19)
    # }
    # if(!any(is.null(colnames(tbl_loc_transm)))) {
    #   legend('topleft',c(colnames(tbl_loc_transm)),col=1:ncol(tbl_loc_transm),pch=19,cex=0.8,ncol=5)
    # }
    
    ## REPRODUCTION NUMBER ----
    data_transm$sim_day[is.na(data_transm$sim_day)] <- 0
    # day of infection: case
    infection_time <- data.frame(local_id      = data_transm$local_id,
                                 infector_id   = data_transm$infector_id,
                                 infection_day = data_transm$sim_day)
    
    # day of infection: infector
    infector_time  <- data.frame(infector_id            = data_transm$local_id,
                                 infector_infection_day = data_transm$sim_day)
    
    
    infection_time$infector_id[is.na(infection_time$infector_id)] <- -1
    #infector_time$infector_id[is.na(infector_time$infector_id)] <- 0
    
    # merge case and infector timings
    infection_time <- merge(infection_time,infector_time, all.x = TRUE)
    
    
    # secondary cases per local_id
    tbl_infections <- table(data_transm$infector_id)
    data_infectors <- data.frame(local_id     = as.numeric(names(tbl_infections)),
                                 sec_cases    = as.numeric(tbl_infections))
    
    # merge secondary cases with time of infection
    sec_transm    <- merge(infection_time,data_infectors,all=T)
    sec_transm$sec_cases[is.na(sec_transm$sec_cases)] <- 0
    sec_transm$infection_day_date <- sim_day_date[sec_transm$infection_day+1]
    
    # plot
    mean_sec_cases <- aggregate(sec_cases ~ infection_day_date, data = sec_transm,mean)
    # remove last 14 days, since the infectious period of these cases is not fully covered
    mean_sec_cases$sec_cases[(nrow(mean_sec_cases)-14):nrow(mean_sec_cases)] <- NA
    plot_ymax <- range(c(0,4,mean_sec_cases$sec_cases),na.rm = T)
    plot(mean_sec_cases,type='b',
         xlab='day of infection',ylab='secondary infections',
         main='reproduction number',
         ylim=plot_ymax,
         xaxt='n')
    axis(1,pretty(sim_day_date),format(pretty(sim_day_date),'%d %b'))
    
    abline(v=sim_day_date[sim_day_date == '2020-03-14'])
    abline(v=sim_day_date[sim_day_date == Sys.Date()])
    
    ## GENERATION INTERVAL   ----
    # note: the generation interval is the time between the infection time of an infected person and the infection time of his or her infector.
    # reference: Kenah et al (2007)
    # remove the generation intervals counted form the initial infected seed infections
    #sec_transm$infector_infection_day[sec_transm$infector_infection_day == -1] <- NA
    sec_transm$generation_interval <- sec_transm$infection_day - sec_transm$infector_infection_day
    
    gen_interval <- sec_transm[!is.na(sec_transm$generation_interval),]
    #gen_interval[gen_interval$infection_day==18,]
    if(nrow(gen_interval)==0) gen_interval <- data.frame(matrix(rep(0,6),nrow=1)); names(gen_interval) <- names(sec_transm)
    mean_generation_interval <- aggregate(generation_interval ~ infection_day_date, data = gen_interval,mean)
    plot(mean_generation_interval,type='b',
         xlab='day of infection',ylab='generation interval [infection]',
         main='generation interval\n[infection]',
         xaxt='n')
    axis(1,pretty(sim_day_date),format(pretty(sim_day_date),'%d %b'))
    abline(h=5.2)
    if(unique(project_summary$track_index_case[flag_exp]) == 'true'){
      text(0,pos=4,'TRACK INDEX CASE ON == NO TERTIARY CASES')
    }
    
    # boxplot(gen_interval$generation_interval, outline = T,
    #         ylab='generation interval [infection]',
    #         main='generation interval\n[infection]')
    # if(unique(project_summary$track_index_case[flag_exp]) == 'true'){
    #   text(0,pos=3,'TRACK INDEX CASE ON == NO TERTIARY CASES')
    # }
    
    
    if(save_pdf) par(mfrow=c(2,2))
    # AGE: SYMPTOMATIC     ----
    barplot(t(table(data_transm$part_age,!is.na(data_transm$start_symptoms))),
            xlab='age of a case',
           # ylim = c(0,0.025),
            main = 'Incidence by age')
    legend('topright',
           c('Symptomatic','Asymptomatic'),
           fill=rev(grey.colors(2)),
           cex=0.8)
    
    table(data_transm$part_age,is.na(data_transm$start_symptoms))
    
    # AGE: HOSPITAL     ----
    hosp_age_breaks <- c(0,18,60,79,110)
    
    data_transm$hosp_age_cat <- cut(data_transm$part_age,hosp_age_breaks,include.lowest = T,right = T)
    
    age_cat_hospital <- data_transm$hosp_age_cat[!is.na(data_transm$hospital_admission_start)]
    barplot(table(age_cat_hospital)/length(age_cat_hospital),
            ylab='Relative frequency hospital admissions',
            xlab='Age category (years)',
            ylim = c(0,0.6),
            main='Relative frequency hospital admissions by age')
    
    
    data_transm$num_hosp <- !is.na(data_transm$hospital_admission_start)
    num_hosp_age_time <- aggregate(num_hosp ~ hosp_age_cat + sim_day + sim_day_date, data= data_transm, sum)
    num_hosp_age_time$hosp_age_cat <- factor(num_hosp_age_time$hosp_age_cat)
    
    plot(range(num_hosp_age_time$sim_day_date),c(0,1),col=0,
         xlab = 'Time',
         ylab = 'Relative frequency hospital admissions',
         main='Hospital admissions over time')
    for(i_day in 1:max(num_hosp_age_time$sim_day)){
      
      num_hosp_age <- num_hosp_age_time[num_hosp_age_time$sim_day == i_day,]
      points(x = num_hosp_age$sim_day_date,
             y = num_hosp_age$num_hosp/sum(num_hosp_age$num_hosp),
             col= as.numeric(num_hosp_age$hosp_age_cat),
             pch = 15)
    }
    legend('top',
           levels(num_hosp_age_time$hosp_age_cat),
           col = 1:nlevels(num_hosp_age_time$hosp_age_cat),
           pch=15,
           ncol=nlevels(num_hosp_age_time$hosp_age_cat),
           title='Age group (years)')
    
    boxplot(data_transm$hospital_admission_start ~ data_transm$hosp_age_cat,
            xlab='Age category (years)',
            ylab='Days',
            main='Time to hospital admission\nsince symptom onset')
    
    
    ## (A)SYMPTOMATIC TRANSMISSION    ----
    tbl_symptomatic_transmission <- table(data_transm$infector_is_symptomatic)
    barplot(tbl_symptomatic_transmission/sum(tbl_symptomatic_transmission),
            ylab='fraction',
            xlab='symptomatic transmission?',
            ylim=0:1,
            main="SYMPTOMATIC TRANSMISSION")
    grid(nx=NA,ny=NULL)  
    
    ## LOCATION       ----
    data_transm$cnt_location[data_transm$cnt_location == 'Household'] <- 'HH'
    data_transm$cnt_location[data_transm$cnt_location == 'PrimaryCommunity'] <- 'com wknd'
    data_transm$cnt_location[data_transm$cnt_location == 'SecondaryCommunity'] <- 'com week'
    data_transm$cnt_location[data_transm$cnt_location == 'K12School'] <- 'school'
    
    # rename the cnt location for the "seed infected"
    num_transm_events <- nrow(data_transm)
    tbl_location <- table(data_transm$cnt_location)
    if(nrow(tbl_location)==0) tbl_location <- matrix(0,nrow=1,ncol=1,dimnames=list(0))
    barplot(tbl_location/num_transm_events,las=2,
            main='tranmission context',ylab='relative incidence',
            ylim=c(0,0.5))
    
    tbl_loc_transm  <- table(data_transm$sim_day,data_transm$cnt_location)
    if(nrow(tbl_loc_transm)==0) tbl_loc_transm <- matrix(0,nrow=1,ncol=1,dimnames=list(0))
    tbl_loc_transm[tbl_loc_transm==0] <- NA
    
    incidence_days      <- as.numeric(row.names(tbl_loc_transm))
    incidence_day_total <- rowSums(tbl_loc_transm,na.rm = T)
    
  } # end for-loop to vary the input_opt_design
  
  # close PDF stream
  if(save_pdf) dev.off()
  
  # command line message
  smd_print('INSPECTION OF TRANSMISSION DATA COMPLETE')
  
} # function end


