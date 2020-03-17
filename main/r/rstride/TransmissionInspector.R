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

#############################################################################
# EXPLORE TRANSMISSION EVENTS, OUTBREAKS GENERATION INTERVALS AND R0       ##
#############################################################################
inspect_transmission_data <- function(project_dir)
{
  # command line message
  smd_print('INSPECT TRANSMISSION DYNAMICS...')
  
  # # inspect outbreaks (separate function)
  # inspect_outbreak_size(project_dir)
  
  # load project summary
  project_summary      <- .rstride$load_project_summary(project_dir)
  
  # retrieve all variable model parameters
  input_opt_design     <- .rstride$get_variable_model_param(project_summary)
  
  # get all transmission output
  data_transm_all      <- .rstride$load_aggregated_output(project_dir,'data_transmission')
  
  # open pdf stream
  .rstride$create_pdf(project_dir,'transmission_inspection',10,7)

  i_config <- 1
  for(i_config in 1:nrow(input_opt_design)){
    
    # reset figure arrangements... and start new plot
    par(mfrow=c(3,3))
    
    # subset transmission output corresponding the 'input_opt_design' row
    flag_exp            <- .rstride$get_equal_rows(project_summary,input_opt_design[i_config,])
    data_transm         <- data_transm_all[data_transm_all$exp_id %in% project_summary$exp_id[flag_exp],]
    num_runs_exp        <- sum(flag_exp)
    num_infected_seeds  <- sum(is.na(data_transm$infector_id)) / num_runs_exp
    
    # get population size
    pop_size            <- unique(project_summary$population_size[flag_exp])
    pop_size_belgium    <- 11.6e6
    pop_factor_100k     <- 1/pop_size*1e5
    pop_factor_belgium  <- 1/pop_size*pop_size_belgium
    
    # get the simulated dates
    # TODO: include checks!
    start_date   <- as.Date(unique(project_summary$start_date[flag_exp]),'%Y-%m-%d')
    num_days     <- unique(project_summary$num_days[flag_exp])
    sim_day_date <- seq(start_date,start_date+num_days,1)
    data_transm$sim_day_date <- sim_day_date[data_transm$sim_day+1]
    
    # INCIDENCE
    tbl_transm        <- table(data_transm$sim_day,data_transm$exp_id)
    tbl_transm_matrix <- matrix(c(tbl_transm),nrow=nrow(tbl_transm),dimnames=list(rownames(tbl_transm)))
    tbl_transm_date   <- sim_day_date[as.numeric(row.names(tbl_transm_matrix))]
    if(nrow(tbl_transm_matrix)==0) tbl_transm_matrix <- matrix(0,nrow=1,ncol=1,dimnames=list(0))
    boxplot(t(tbl_transm_matrix)*pop_factor_100k,
            at=tbl_transm_date,
            main='Incidence: per day / 100k',
            xlab='time (days)',ylab='New cases (per 100k)',
            xaxt='n')
    axis(1,pretty(tbl_transm_date),format(pretty(tbl_transm_date),"%d %b"))
    grid()
    legend_info <- c(paste('num. runs:',num_runs_exp),
                     paste('inf. seeds / run:',num_infected_seeds))
    
    if(nrow(input_opt_design)>0){ # add some param info to the legend
      legend_info <- c(legend_info,paste0(colnames(input_opt_design),': ',input_opt_design[i_config,]))
    }
    legend('topleft',legend_info,cex=0.8,bg='white')
    
    # CUMMULATIVE INCIDENCE: AVERAGE
    boxplot(t(apply(tbl_transm_matrix,2,cumsum))*pop_factor_100k,
            at=tbl_transm_date,
            xlab='Date',ylab='Cummulative incidence (per 100k)',
            main='Incidence: cummulative (per 100k)',
            xaxt='n')
    axis(1,pretty(tbl_transm_date),format(pretty(tbl_transm_date),"%d %b"))
    grid()
    
    
    # LOCATION
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
    
    # plot(range(incidence_days),c(0,1.4),col=0,xlab='day',ylab='relative daily incidence',
    #      main='transmission context over time',yaxt='n')
    # axis(2,seq(0,1,0.2),cex.axis=0.9)
    # for(i_loc in 1:ncol(tbl_loc_transm)){
    #   points(incidence_days, tbl_loc_transm[,i_loc]/incidence_day_total,col=i_loc,lwd=4,pch=19)
    # }
    # if(!any(is.null(colnames(tbl_loc_transm)))) {
    #   legend('topleft',c(colnames(tbl_loc_transm)),col=1:ncol(tbl_loc_transm),pch=19,cex=0.8,ncol=5)
    # }
    
    # REPRODUCTION NUMBER
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
    plot_xlim <- range(c(0,sec_transm$infection_day),na.rm=T)
    plot_ymax <- range(c(0,4,mean_sec_cases$sec_cases))
    plot(mean_sec_cases,type='b',
         xlab='day',ylab='secondary infections',
         main='reproduction number',
         ylim=plot_ymax,
         xaxt='n')
    axis(1,pretty(sim_day_date),format(pretty(sim_day_date),'%d %b'))
    
    ## GENERATION INTERVAL
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
         xlab='day',ylab='generation interval [infection]',
         main='generation interval\n[infection]',
         xaxt='n')
    axis(1,pretty(sim_day_date),format(pretty(sim_day_date),'%d %b'))
    
    if(unique(project_summary$track_index_case[flag_exp]) == 'true'){
      text(0,pos=4,'TRACK INDEX CASE ON == NO TERTIARY CASES')
    }
    
    boxplot(gen_interval$generation_interval, outline = T,
            ylab='generation interval [infection]',
            main='generation interval\n[infection]')
    if(unique(project_summary$track_index_case[flag_exp]) == 'true'){
      text(0,pos=3,'TRACK INDEX CASE ON == NO TERTIARY CASES')
    }
    
    # ## OUTBREAKS
    # data_transm$outbreak_id <- as.numeric(as.factor(data_transm$id_index_case))
    # outbreak_size_data      <- as.numeric(table(data_transm$outbreak_id))
    # outbreak_size_breaks    <- c(1,2,5,10,20,50,max(c(100,outbreak_size_data+1)))
    # outbreak_size_cat       <- cut(outbreak_size_data,breaks=outbreak_size_breaks,right = F)
    # 
    # # adjust labels
    # levels(outbreak_size_cat) <- paste0('[',outbreak_size_breaks[-length(outbreak_size_breaks)],',',outbreak_size_breaks[-1]-1,']')
    # levels(outbreak_size_cat)[1] <- "[1]"
    # levels(outbreak_size_cat)[6] <- "[50,+]"
    # 
    # # plot
    # percentage_plot <- round(table(outbreak_size_cat)/length(outbreak_size_cat)*100)
    # bplot <- barplot(percentage_plot,
    #                  ylab='%',
    #                  xlab='outbreak size',
    #                  main=paste0('outbreak size (n:',length(outbreak_size_data),')'),
    #                  las=2,
    #                  ylim=c(0,100),
    #                  cex.names = 0.8)
    # # add some info to the legend
    # num_infected_seeds <- length(outbreak_size_data) / length(unique(data_transm$exp_id))
    # legend('top',c(paste('num. runs',num_runs_exp),paste('outbreaks / run',num_infected_seeds)),cex=0.8)
    # # add values
    # label_plot    <- paste0(percentage_plot,'%')
    # text(bplot,percentage_plot,label_plot,pos=3)
    # 
    # # add distribution
    # boxplot(outbreak_size_data,
    #         main='outbreak size (all)')
    # points(1.3,mean(outbreak_size_data),pch=4,lwd=3)
    # text(1.3,mean(outbreak_size_data),
    #      paste0('mean\n(',round(mean(outbreak_size_data),digits=2),')'),pos=3)
    # 
    # bxplot <- boxplot(outbreak_size_data,
    #         main='outbreak size (95% CI)',
    #         outline = F)
    # bool_outlier <- outbreak_size_data %in% bxplot$out
    # points(1.3,mean(outbreak_size_data[!bool_outlier]),pch=4,lwd=3)
    # text(1.3,mean(outbreak_size_data[!bool_outlier]),
    #      paste0('mean\n(',round(mean(outbreak_size_data[!bool_outlier]),digits=2),')'),pos=3)
    # 
    # # count / day
    # tbl_all <- table(data_transm$sim_day+1,data_transm$outbreak_id)
    # 
    # # insert this in standard matrix: [sim_day;outbreak_id]
    # tbl_all_matrix <- matrix(0,nrow=max(project_summary$num_days),ncol=max(data_transm$outbreak_id))
    # tbl_all_matrix[as.numeric(row.names(tbl_all)),] <- tbl_all
    # rownames(tbl_all_matrix) <- 0:max(project_summary$num_days-1)
    # 
    # # take the inverse for the cummulative sum
    # # and analyse the outbreaks over time
    # tbl_all_inv  <- tbl_all_matrix[nrow(tbl_all_matrix):1,] 
    # if(is.null(dim(tbl_all_inv))){
    #   tbl_all_cum  <- cumsum(tbl_all_inv)
    #   outbreaks_over_time <- data.frame(day = 1:length(tbl_all_cum),
    #                                     count = tbl_all_cum)
    # } else {
    #   tbl_all_cum  <- apply(tbl_all_inv,2,cumsum)
    #   outbreaks_over_time <- data.frame(day = as.numeric(rownames(tbl_all_cum)),
    #                                     count = rowSums(tbl_all_cum > 0))
    # }
    # 
    # 
    # # plot outbreak size over time
    # for(i in 2:length(levels(outbreak_size_cat))){
    #   if(sum(as.numeric(outbreak_size_cat) == i) > 1){
    #     tbl_selection <- t(apply(tbl_all[,as.numeric(outbreak_size_cat) == i],2,cumsum))  
    #   } else {
    #     tbl_selection <- t(cumsum(tbl_all[,as.numeric(outbreak_size_cat) == i]))
    #   }
    #   if(ncol(tbl_selection) > 0){
    #     boxplot(tbl_selection,
    #             at=as.numeric(colnames(tbl_selection)),
    #             ylab='cummulative cases / outbreak',
    #             main=paste0('cummulative cases / outbreak \nsize: ',levels(outbreak_size_cat)[i]),
    #             xlab='time (day)')
    #     legend('topleft',
    #            paste0('absolute count: ',nrow(tbl_selection)),
    #            cex=0.8)
    #   }
    # }
    # 
    
    # # plot the number of ongoing outbreaks over time
    # plot(outbreaks_over_time$day,outbreaks_over_time$count,
    #      ylim=c(0,max(data_transm$outbreak_id)),
    #      xlab='day of last infection',ylab='count (oubtreaks)',
    #      main='number ongoing outbreaks over time')
    
    # cases by age
    # names(data_transm)
    # hist(data_transm$part_age,unique(data_transm$part_age),right = F,
    #      freq = F,xlab='age of a case',
    #      ylim = c(0,0.025),
    #      main = 'incidence by age')  
    
    # cases by age group
    hist(data_transm$part_age,seq(0,100,5),right = F,
         freq = F,
         xlab='age of a case',
         ylim = c(0,0.025),
         main = 'incidence by age')  
    
    # # plot age-interval per outbreak
    # data_outbreak_tmp <- data_transm
    # data_outbreak_tmp$sim_day[is.na(data_outbreak_tmp$sim_day)] <- -10
    # bymedian <- with(data_outbreak_tmp, reorder(outbreak_id, -part_age, median))
    # boxplot(part_age ~ bymedian, data = data_outbreak_tmp,
    #         xlab='outbreak id (sorted by the median age)',ylab='age of the cases')
    # 
    # # plot age-interval over time
    # boxplot(part_age ~ sim_day_date, data=data_transm,
    #         at=sort(unique(data_transm$sim_day_date)),
    #         xlab='time (days)',ylab='age of the cases',
    #         cex=0.8,
    #         xaxt='n')  
    # axis(1,pretty(sim_day_date),format(pretty(sim_day_date),'%d %b'))
    # 
    # ###############################
    # # incidence per age group  
    # ###############################
    # 
    # # get age distribution in the (secondary) cases
    # age_breaks <- seq(0,100,10)
    # data_case_age     <- cut(data_transm$part_age,age_breaks,right=F)
    # #data_sec_case_age <- cut(data_transm$part_age[data_transm$sim_day>0],age_breaks,right=F)
    # 
    # # define the number of outbreaks
    # num_outbreaks <- sum(data_transm$sim_day==0)
    # 
    # # calculate the incidence 
    # inc_case_age     <- table(data_case_age) / num_outbreaks
    # #inc_sec_case_age <- table(data_sec_case_age) / num_outbreaks
    # plot_ylim <- range((c(0,0.5,inc_case_age,inc_sec_case_age)*1.2))
    # 
    # # plot the incidence per age group
    # barplot(inc_case_age,
    #         las=2,xlab='age',ylab='Incidence',cex.names=0.8,
    #         ylim=plot_ylim, main = 'Incidence per outbreak\nby age group')
    # 
    # # # plot the secondary incidence per age group
    # # barplot(inc_sec_case_age,
    # #         las=2,xlab='age',ylab='Secondary incidence',cex.names=0.8,
    # #         ylim=plot_ylim, main = 'Secondary incidence per outbreak \nby age group')
    # 
    
    # PREDICTIONS FOR BELGIUM
    
    # DAILY INCIDENCE
    inc_belgium <- t(tbl_transm_matrix)*pop_factor_belgium/1e3
    plot_ylim <- range(inc_belgium,80)
    plot(tbl_transm_date,colMeans(inc_belgium),
         type='l',lwd=3,
            main='Incidence: per day (Belgium, mean)',
            xlab='time (days)',ylab='New cases (1000x)',
            ylim = plot_ylim)
    grid()
    
    # CUMMULATIVE INCIDENCE: predict for Belgium
    tbl_transm_matrix
    inc_cum_belgium <- t(apply(tbl_transm_matrix,2,cumsum))*pop_factor_belgium/1e5
    plot_ylim <- range(inc_cum_belgium,30)
    plot(tbl_transm_date,colMeans(inc_cum_belgium),type='l',lwd=3,
            xlab='Date',ylab='Cummulative incidence (100k)',
            main='Incidence: cummulative (Belgium)',
            #xaxt='n',
            ylim = plot_ylim)
    axis(1,pretty(tbl_transm_date),format(pretty(tbl_transm_date),"%d %b"))
    grid()
    
  } # end for-loop to vary the input_opt_design
  
  # close PDF stream
  dev.off()
  
  # command line message
  smd_print('INSPECTION OF TRANSMISSION DATA COMPLETE')
  
} # function end


inspect_outbreak_size <- function(project_dir){
  
  # load project summary
  project_summary      <- .rstride$load_project_summary(project_dir)
  
  # retrieve all variable model parameters
  input_opt_design     <- .rstride$get_variable_model_param(project_summary)
  input_opt_param      <- names(input_opt_design)
  
  # add parameter tag
  preferred_order <- c('r0','vaccine_rate','vaccine_profile','num_days')
  preferred_order <- preferred_order[preferred_order %in% input_opt_param]
  preferred_order <- c(preferred_order,input_opt_param[!input_opt_param %in% preferred_order])
  project_summary$param_tag <- apply(project_summary[,preferred_order],1,paste0,collapse='_')

  # open pdf stream
  .rstride$create_pdf(project_dir,'outbreak_inspection',10,7)
  par(mar =c(10,5,3,1))
  
  data_transm         <- .rstride$load_aggregated_output(project_dir,'data_transmission')
  
  # add (dummy) column
  data_transm$outbreak_size <- 1
  
  # aggregate
  outbreak_size_data <- aggregate(outbreak_size ~ id_index_case + exp_id, data= data_transm, sum)
  
  # add model parameter info
  outbreak_size_data <- merge(outbreak_size_data,project_summary)

  # create comparison function
  param_fction <- formula(paste(c('outbreak_size ~ param_tag'),collapse=''))
  param_fction
  
  size_mean_all <- aggregate(param_fction,data = outbreak_size_data,mean)
  
  #upper whisker = min(max(x), Q_3 + 1.5 * IQR) 
  #lower whisker = max(min(x), Q_1 – 1.5 * IQR)
  
  size_q1 <- aggregate(param_fction,data = outbreak_size_data,quantile,0.25)$outbreak_size
  size_q3 <- aggregate(param_fction,data = outbreak_size_data,quantile,0.75)$outbreak_size
  size_min <- aggregate(param_fction,data = outbreak_size_data,min)$outbreak_size
  size_max <- aggregate(param_fction,data = outbreak_size_data,max)$outbreak_size
  
  IQR <- size_q3 - size_q1
  size_upper <- apply(cbind(size_max,size_q3 + 1.5*IQR),1,min)
  size_lower <- apply(cbind(size_min,size_q1 - 1.5*IQR),1,max)
  
  boxplot(param_fction,
          data = outbreak_size_data,
          main='outbreak size',
          ylab='size',las=2);grid()
  points(x = 1:nrow(size_mean_all),
         y = size_mean_all$outbreak_size,
         pch = 4,
         lwd=3,
         col='blue')
  legend('topleft','mean',pch=4,lwd=2,col='blue',lty=0,cex=0.7)
  
  bxplot <- boxplot(param_fction,
                    data = outbreak_size_data,
                    main='outbreak size (no outliers)',
                    outline = F,
                    #ylim = range(c(0,size_mean_all$outbreak_size)),
                    las=2);grid()
  points(x = 1:nrow(size_mean_all),
         y = size_mean_all$outbreak_size,
         pch = 4,
         lwd=3,
         col='blue')
  legend('topleft','mean',pch=4,lwd=2,col='blue',lty=0,cex=0.7)
  
  i <- 1
  size_mean_all$outbreak_size_iqr <- 0
  for(i in 1:nrow(size_mean_all)){
    bool_exp <- outbreak_size_data$param_tag == size_mean_all$param_tag[i] &
                between(outbreak_size_data$outbreak_size,size_lower[i],size_upper[i])
    
    size_mean_all$outbreak_size_iqr[i] <- mean(outbreak_size_data$outbreak_size[bool_exp])
  }
  points(1:nrow(size_mean_all),size_mean_all$outbreak_size_iqr,
         pch=4,col='red',lwd=2)
  points(1:nrow(size_mean_all),size_mean_all$outbreak_size,
         pch=4,col='blue',lwd=2)
  legend('topleft',c('mean (all)','mean (iqr)'),pch=4,lwd=2,col=c(4,2),lty=0,cex=0.7)
  
  
  ## DISTRIBUTION
  names(outbreak_size_data)
  head(outbreak_size_data)
  
  opt_r0  <- unique(outbreak_size_data$r0)
  opt_param_tag <- unique(outbreak_size_data[,c('param_tag','r0','vaccine_rate','vaccine_profile')])
  names(opt_param_tag)
  opt_param_tag <- data.frame((apply(opt_param_tag,2,as.factor)))
  
  opt_param_tag$col = as.numeric(opt_param_tag$param_tag)
  
  #size_bin <- c(1:100,500,1000)
  size_bin <- 10
  
  plot(y = c(0,4),
       x = c(0,20),
       col = 0,
       ylab='log10(Frequency)',
       xlab='outbreak size')
  xlabels <- c(pretty(1:100),'500','1000')
  #axis(1,at=100,'100+')
  pretty(size_bin[1:100])
  
  i_param    <- 2
  for(i_param in 1:nlevels(opt_param_tag$param_tag)){
    
    bool_param <- outbreak_size_data$param_tag == opt_param_tag$param_tag[i_param]
    
    hist_data  <- hist(outbreak_size_data$outbreak_size[bool_param],
                       size_bin,
                       plot = FALSE)
    hist_data$counts[hist_data$counts==0] <- NA
    hist_data$counts = (hist_data$counts)
    
    plot(hist_data$mids, 
          hist_data$counts,
          col = opt_param_tag$col[i_param])
    
  }
  
  dev.off() # close pdf stream
  
  ## MEAN OF MEANS
  
  # open pdf stream
  .rstride$create_pdf(project_dir,'outbreak_mean_sample_random',10,10)
  par(mfrow=c(2,3))
  
  bool_coverage <- outbreak_size_data$vaccine_rate == 0.7 & outbreak_size_data$vaccine_profile == 'Random'
  bool_coverage <- outbreak_size_data$vaccine_rate == 0
  
  outbreak_size_sel <- outbreak_size_data[bool_coverage,]
  
  bin_opt <- c(10,25,50,100,250,1000)
  sample_opt <- 1

  num_bin <- 10
  
  overall_mean <- aggregate(outbreak_size ~ r0, data = outbreak_size_sel, mean) 
  
  for(num_bin in bin_opt){
  for(i_sample in sample_opt){
    
    plot_title <- paste0(num_bin,'x n', nrow(outbreak_size_sel) / num_bin / 3)
  
    outbreak_size_sel$dummy <- paste(outbreak_size_sel$r0,sample(1:num_bin,nrow(outbreak_size_sel),replace = T),sep='_')
    
    sample_means <- aggregate(outbreak_size ~ r0 + dummy, data = outbreak_size_sel, mean)
    nrow(sample_means)/3
    
    boxplot(outbreak_size ~ r0, data = sample_means,
            ylim = c(0,max(c(100,sample_means$outbreak_size))),
            #ylim = c(0,max(c(100,1))),
            main=plot_title,
            ylab='mean outbreak size (count)',
            xlab='R0',
            outline = T)
    points(1:nrow(overall_mean),
           overall_mean$outbreak_size,
           pch=4,
           col=4,
           lwd=2)
  }
  }
  
  # close pdf stream
  dev.off()
  
  
}

