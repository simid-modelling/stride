############################################################################# #
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
############################################################################# #
#
# MODEL INCIDENCE EXPLORATION
#
############################################################################# #

rm(list=ls())

source('./bin/rstride/rStride.R')
# install.packages('Rmisc')
library('Rmisc')


# set work dir
.rstride$set_wd()

# set directory name with results and get output files
#dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200601_results'
#dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200606_results'
dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200608_results'
#dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200608_results_bis'

file_name_incidence <- dir(dir_results,pattern='incidence_processed.RData',recursive = T,full.names = T)

# output tag
output_tag <- paste0(format(Sys.Date(),'%Y%m%d'),'_results')

# remove tracing scenarios
file_name_incidence <- file_name_incidence[!grepl('_tracing',file_name_incidence)]

# remove fitting scenarios
file_name_incidence <- file_name_incidence[!grepl('_fitting',file_name_incidence)]

## LOAD DATA ----
i_file <- 
# load and aggregate results
foreach(i_file = 1:length(file_name_incidence),
        .combine = rbind) %do% {
  
          # load results
          load(file_name_incidence[i_file])
          names(data_incidence_all)
          
          # parse scenario name
          scenario_name <- substr(basename(file_name_incidence[i_file]),16,100)
          scenario_name <- gsub('_incidence_processed.RData','',scenario_name)
          scenario_name <- gsub('_int','scen',scenario_name)
          
          # fix single digit numbers
          if(grepl('scen._',scenario_name)){
            scenario_name <- gsub('scen','scen0',scenario_name)
          }
          
          scenario_name
          
          if(!'location_HouseholdCluster' %in% names(data_incidence_all)){
            data_incidence_all$location_HouseholdCluster <- 0
          }
          
          # remove location NA
          data_incidence_all$location_NA <- NULL
          
          # add scenario name
          data_incidence_all$scenario <- scenario_name
          
          data_incidence_all$scenario_id <- as.numeric(substr(scenario_name,5,6))
          
          # check
          smd_print(i_file,scenario_name)
          smd_print(dim(data_incidence_all))
          
          # return
          data_incidence_all
} -> data_incidence_scenario

# remove delay of 14 days
unique(data_incidence_scenario$config_id)
data_incidence_scenario <- data_incidence_scenario[!grepl('_14',data_incidence_scenario$config_id),]
data_incidence_scenario$config_id <- gsub('_21','',data_incidence_scenario$config_id)
data_incidence_scenario$tracing_id <- gsub('_21','',data_incidence_scenario$tracing_id)

# make copy and add month
data_incidence <- data_incidence_scenario
data_incidence$sim_month <- format(data_incidence$sim_date,'%B')
table(data_incidence$scenario)


# remove data before May 
data_incidence <- data_incidence[data_incidence$sim_date > as.Date("2020-05-01"),]

# remove secondary cases from August 15th
data_incidence$sec_cases[data_incidence$sim_date > as.Date("2020-08-15")] <- NA

# create labels and add colors
data_incidence$scenario_label <- NA
data_incidence$scenario_label[grepl('scen01',data_incidence$scenario)] <- 'Baseline'
data_incidence$scenario_label[grepl('scen02',data_incidence$scenario)] <- 'Baseline w/o B2B'
data_incidence$scenario_label[grepl('scen03',data_incidence$scenario)] <- 'Baseline w/o community'
data_incidence$scenario_label[grepl('scen04',data_incidence$scenario)] <- 'Baseline w/o school*'
data_incidence$scenario_label[grepl('scen05',data_incidence$scenario)] <- 'Baseline w/o PM school*'

data_incidence$scenario_label[grepl('scen06',data_incidence$scenario)] <- 'School 0-5y'
data_incidence$scenario_label[grepl('scen07',data_incidence$scenario)] <- 'School 0-11y'
data_incidence$scenario_label[grepl('scen09',data_incidence$scenario)] <- 'School 0-11y w/o PM'
data_incidence$scenario_label[grepl('scen08',data_incidence$scenario)] <- 'School 0-17y'

data_incidence$scenario_label[grepl('scen10',data_incidence$scenario)] <- 'HH bubbles'
data_incidence$scenario_label[grepl('scen11',data_incidence$scenario)] <- 'HH bubbles: 7/7d'
data_incidence$scenario_label[grepl('scen12',data_incidence$scenario)] <- 'HH bubbles: 2/7d'
data_incidence$scenario_label[grepl('scen13',data_incidence$scenario)] <- 'HH bubbles: gap 20y'
data_incidence$scenario_label[grepl('scen14',data_incidence$scenario)] <- 'HH bubbles: gap 60y'
data_incidence$scenario_label[grepl('scen15',data_incidence$scenario)] <- 'HH bubbles: size 3'
data_incidence$scenario_label[grepl('scen16',data_incidence$scenario)] <- 'HH bubbles: size 4'

data_incidence$scenario_label[grepl('scen17',data_incidence$scenario)] <- 'Baseline with CTS'

# create labels and add colors for Children
data_incidence$scenario_label[grepl('scen21',data_incidence$scenario)] <- 'Baseline (c)'
data_incidence$scenario_label[grepl('scen22',data_incidence$scenario)] <- 'Baseline w/o B2B (c)'
data_incidence$scenario_label[grepl('scen23',data_incidence$scenario)] <- 'Baseline w/o community (c)'
data_incidence$scenario_label[grepl('scen24',data_incidence$scenario)] <- 'Baseline w/o school* (c)'
data_incidence$scenario_label[grepl('scen25',data_incidence$scenario)] <- 'Baseline w/o PM school* (c)'

data_incidence$scenario_label[grepl('scen26',data_incidence$scenario)] <- 'School 0-5y (c)'
data_incidence$scenario_label[grepl('scen27',data_incidence$scenario)] <- 'School 0-11y (c)'
data_incidence$scenario_label[grepl('scen29',data_incidence$scenario)] <- 'School 0-11y w/o PM (c)'
data_incidence$scenario_label[grepl('scen28',data_incidence$scenario)] <- 'School 0-17y (c)'

data_incidence$scenario_label[grepl('scen30',data_incidence$scenario)] <- 'HH bubbles (c)'
data_incidence$scenario_label[grepl('scen37',data_incidence$scenario)] <- 'Baseline with CTS (c)'

## Scenario name and color
opt_scenario    <- data.frame(unique(data_incidence[,c('scenario','scenario_label','scenario_id')]),
                              col_value = 1,
                              stringsAsFactors = F)
       
opt_scenario <- opt_scenario[order(opt_scenario$scenario),]
                              
#flag_baseline   <- grepl('Baseline',data_incidence$scenario_label)
opt_scenario$col_value[grepl('School ',opt_scenario$scenario_label)] <- 3
opt_scenario$col_value[grepl('HH bubbles',opt_scenario$scenario_label)] <- 5
opt_scenario$col_value[opt_scenario$col_value == 5 & !grepl(':',opt_scenario$scenario_label)] <- 4
opt_scenario$col_value[grepl('CTS',opt_scenario$scenario_label)] <- 2

opt_scenario$col <- alpha(opt_scenario$col_value,0.7)

# reference baseline?
opt_scenario$reference_id <- 1
opt_scenario$reference_id[grepl('\\(c\\)',opt_scenario$scenario_label)] <- 21

opt_scenario$reference_index <- which(opt_scenario$scenario_label == 'Baseline')
opt_scenario$reference_index[grepl('\\(c\\)',opt_scenario$scenario_label)] <- which(opt_scenario$scenario_label == 'Baseline (c)')

head(opt_scenario)

# add scenario config to incidence data
data_incidence <- merge(data_incidence,opt_scenario)


## REFERENCE DATA COVID-19: new hospitalisation ----
hosp_adm_data      <- get_observed_incidence_data()
hosp_adm_data$date <- as.Date(hosp_adm_data$sim_date)
hosp_adm_data$num_adm <- hosp_adm_data$hospital_admissions
hosp_adm_data$cum_adm <- hosp_adm_data$cumulative_hospital_admissions

## PREVALENCE DATA STOCHASTIC MODEL
prevalence_ref <- readRDS('./data/prevalence_stochastic_model_20200604.rds')
head(prevalence_ref)
pop_size_be <- 11e6  #TODO: use universal variable



#bxplt_data <- bxplt_data[bxplt_data$scenario_id %in% c(1,2,7,8,9,11,14),]
bxplt_data <- data_incidence;
# bxplt_data <- data_incidence_sel
colname_out <- 'new_hospital_admissions'; y_limits <- c(0,1400); y_label <- 'debug'
#colname_out <-  'cumulative_hospital_cases';y_limits <- c(15,90)*1e3; y_label <- 'debug'; bxplt_data <- bxplot_cumulative
plot_montly_stats_vertical <- function(bxplt_data, colname_out,y_limits,y_label){
  
  opt_scenario_plot <- unique(bxplt_data[,c('scenario','scenario_label','scenario_id','col')])
  # opt_month <- c('May','July','August')
  #opt_month <- c('June','August')
  opt_month <- unique(bxplt_data$sim_month)
  
  # sort months
  opt_month <- opt_month[order(as.Date(paste(1,opt_month),'%d %B'))]
  
  #par(mfrow=c(1,length(opt_month)),mar=c(10,4,4,1))
  par(mar=c(10,4,2,1))
  i_month <- opt_month[4]
  for(i_month in opt_month){
    flag_date <- bxplt_data$sim_month == i_month
    
    y_lim_plot <- y_limits
    if(any(is.na(y_limits))){
      y_lim_plot <- range(bxplt_data[flag_date,colname_out],na.rm=T)
    }
    bplt <- boxplot(formula(paste(colname_out, '~ scenario')), 
            data = bxplt_data[flag_date,],
            outline = F,
            las=2,
            ylim=y_lim_plot,
            ylab = y_label,
            col = (opt_scenario_plot$col),
            main=i_month,
            yaxt='n',
            xaxt='n')
  
    if(grepl('Reproduction',y_label)){
      abline(h=1,lty=3) # for reproduction number
    }
    
    grid(nx=NA,ny=NULL)
    
    # y-axis
    y_ticks <- pretty(y_lim_plot)
    y_labels <- y_ticks
    if(min(y_lim_plot)>1e3){ y_labels <- paste0(y_ticks/1e3,'k') }
    axis(2,y_ticks,y_labels,las=2)

    # x-axis
    text_x <- seq(1, nrow(opt_scenario_plot), by = 1)
    text_y <- par("usr")[3] * ifelse(par("usr")[3]>0,0.95,1.3)
    #print(text_y)
    text(text_x, text_y,
         srt = 60, adj = 1, xpd = TRUE,
         labels = opt_scenario_plot$scenario_label)
    axis(1,1:nrow(opt_scenario_plot),rep('',nrow(opt_scenario_plot)),tcl=0.5)

    # add scenario numbers
    # text(seq(1, nrow(opt_scenario_plot), by = 1), bplt$stats[5,],
    #      xpd = TRUE, pos=3,
    #      labels = opt_scenario_plot$scenario_id)

    
    
    # get scenario averages
    out_mean <- aggregate(formula(paste(colname_out, '~ scenario + scenario_id + scenario_id + reference_id')),data = bxplt_data[flag_date,],mean)
    #out_mean <- aggregate(formula(paste(colname_out, '~ scenario + scenario_id + scenario_id + reference_id')),data = bxplt_data[flag_date,],max)
    
    # get scenario reference
    out_mean$reference_index <- NA
    for(i in 1:nrow(out_mean)){
      out_mean$reference_index <- which(out_mean$scenario_id == out_mean$reference_id[i])
    }
    
    # add average and relative difference
    out_mean$relative <- round(out_mean[,colname_out] / out_mean[out_mean$reference_index,colname_out] *100)
    points(seq(1, nrow(out_mean)),out_mean[,colname_out],pch='+',cex=0.7)
    text(seq(1, nrow(opt_scenario_plot), by = 1), bplt$stats[5,],
         xpd = TRUE, pos=3,
         labels = paste0('[',opt_scenario_plot$scenario_id,']\n',out_mean$relative,'%'),
         #labels = paste0(out_mean$relative,'%'),
         cex=0.5)
    
  } # end for-loop 'month'
  

} # end function

### POLYGONS ---- 
#Hospital admissions

polygon_legend_cex <- 0.8
#colname_burden <- 'sec_cases'
add_polygon <- function(scen_tag,colname_burden, scen_color,data_incidence){
  data_incidence <- data_incidence[grepl(scen_tag,data_incidence$scenario),]
  hosp_min       <- aggregate(formula(paste(colname_burden,'~ sim_date')), data=  data_incidence, min)
  hosp_max       <- aggregate(formula(paste(colname_burden,'~ sim_date')), data=  data_incidence, max)
  # hosp_min       <- aggregate(formula(paste(colname_burden,'~ sim_date')), data=  data_incidence, quantile,0.025)
  # hosp_max       <- aggregate(formula(paste(colname_burden,'~ sim_date')), data=  data_incidence, quantile,0.975)
  hosp_median    <- aggregate(formula(paste(colname_burden,'~ sim_date')), data=  data_incidence, median,na.rm=T)
  hosp_mean      <- aggregate(formula(paste(colname_burden,'~ sim_date')), data=  data_incidence, mean,na.rm=T)
  newx           <- c(hosp_min$sim_date,rev(hosp_max$sim_date))
  newy           <- c(hosp_min[,2],rev(hosp_max[,2]))
  polygon(newx, newy, col = alpha(scen_color,0.4), border = alpha(scen_color,0.4))
  #lines(hosp_median$sim_date,hosp_median$new_hospital_admissions,type='l',lwd=2,col=scen_color)
  #lines(hosp_mean$sim_date,hosp_mean$new_hospital_admissions,type='l',lwd=2,col=scen_color,lty=3)
  
  hosp_summary <- merge(hosp_median,hosp_mean,by='sim_date',suffixes = list('_median','_mean'))
  
  return(hosp_summary)
}


# plot_main <- 'debug'
# data_incidence <- data_incidence_scenario
# scen_tag <- 'scen30'
# scen_color <- 7
get_incidence_reproduction_plot <- function(hosp_adm_data,plot_main,scen_tag,scen_color,data_incidence)
{
  y_lim  <- c(0,700)
  x_lim  <- range(data_incidence_scenario$sim_date)
  
  par(fig=c(0,1,0.35,1),mar=c(0,5,2,1))
  plot(hosp_adm_data$date,hosp_adm_data$num_adm,pch=20,
       xaxt='n', yaxt='n',
       xlab = '',ylab='Hospital admissions',
       ylim = y_lim,
       xlim = x_lim,
       main=plot_main)
  sum_scen1 <- add_polygon(scen_tag,'new_hospital_admissions',scen_color,data_incidence)
  add_y_axis(y_lim)
  add_breakpoints()
  
  par(fig=c(0,1,0,0.34),mar=c(3,5,0,1), new=TRUE)
  plot(0,0,pch=20,
       xaxt='n',yaxt='n',
       xlab = '',ylab='Re',
       ylim = c(0,4.1),
       xlim = x_lim,
       main='')
  add_polygon(scen_tag,'sec_cases',scen_color,data_incidence)
  add_x_axis(data_incidence$sim_date)
  add_y_axis(0:4)
  add_breakpoints(bool_text=FALSE)
  
}


# data_incidence <- data_incidence_base
# date_start_str <- '2020-03-07'
# date_end_str <- '2020-03-13'
print_transmission_summary <- function(data_incidence, date_start_str, date_end_str,num_exp = 10){
  
  sel_dates    <- (as.Date(date_start_str):as.Date(date_end_str))
  flag_date    <- data_incidence$sim_date %in% sel_dates & data_incidence$exp_id %in% 1:num_exp

  # remove doubling times that are based on data beyond the selected period
  data_incidence$doubling_time[data_incidence$sim_date + data_incidence$doubling_time  > max(sel_dates)] <- NA

  # help function for CI with NA's
  rm_na <- function(x){
    return(x[!is.na(x)])
  }
  
  print(paste0('GEN. INTERVAL (',date_start_str,' : ',date_end_str,')'))
  print(round(CI(x=rm_na(data_incidence$gen_interval[flag_date]),ci=0.95),digits=2)[c(2,3,1)])
  
  print(paste0('DOUBLING TIME (',date_start_str,' : ',date_end_str,')'))
  print(round(CI(x=rm_na(data_incidence$doubling_time[flag_date]),ci=0.95),digits=2)[c(2,3,1)])
  
  print(paste0('REPRODUCTION NUMBER (',date_start_str,' : ',date_end_str,')'))
  print(round(CI(x=rm_na(data_incidence$sec_cases[flag_date]),ci=0.95),digits=2)[c(2,3,1)])
}

## POLYGONS FULL ----

# reproduction number => remove burn-in and last 2 weeks
#data_incidence_scenario$sec_cases[data_incidence_scenario$sim_date > max(data_incidence_scenario$sim_date)-14] <- NA
#data_incidence_scenario$sec_cases[data_incidence_scenario$sim_date < min(data_incidence_scenario$sim_date)+14] <- NA

pdf(smd_file_path(dir_results,paste0(output_tag,'_manuscript_polygon_full.pdf')),4,4)

scen_opt <- rbind(c('scen01','scen10','scen17'), # child = adult
                  c('scen21','scen30','scen37')) # child = 1/2 adult

if(!any(21 %in% data_incidence_scenario$scenario_id)){
  scen_opt <- t(as.matrix(scen_opt[1,]))
}

for(i in 1:nrow(scen_opt)){
  
  get_incidence_reproduction_plot(hosp_adm_data,
                                  plot_main = 'Baseline',
                                  scen_tag = scen_opt[i,1],
                                  scen_color = 1,
                                  data_incidence = data_incidence_scenario)
  
  get_incidence_reproduction_plot(hosp_adm_data,
                                  plot_main = 'Household Bubbles (size 2, 4/7d)',
                                  scen_tag = scen_opt[i,2],
                                  scen_color = 4,
                                  data_incidence = data_incidence_scenario)
  
  get_incidence_reproduction_plot(hosp_adm_data,
                                  plot_main = 'Contact Tracing Strategy (CTS)',
                                  scen_tag = scen_opt[i,3],
                                  scen_color = 2,
                                  data_incidence = data_incidence_scenario)
}

dev.off()

## POLYGONS BUBBLES ----

# reproduction number => remove burn-in and last 2 weeks
#data_incidence_scenario$sec_cases[data_incidence_scenario$sim_date > max(data_incidence_scenario$sim_date)-14] <- NA
#data_incidence_scenario$sec_cases[data_incidence_scenario$sim_date < min(data_incidence_scenario$sim_date)+14] <- NA

pdf(smd_file_path(dir_results,paste0(output_tag,'_manuscript_polygon_bubble.pdf')),4,4)

scen_opt <- paste0('scen',c('01',10:16)) # child = 1/2 adult


unique(data_incidence_scenario$contact_id)

i <- 1

opt_cnt <- c(',15',',20')

for(i in 1:length(scen_opt)){

  flag <- grepl(scen_opt[i],data_incidence_scenario$scenario)
  data_incidence_scenario[flag,]
  scenario_id    <- unique(data_incidence_scenario$scenario_id[flag])
  scenario_label <- unique(data_incidence$scenario_label[data_incidence$scenario_id == scenario_id])
  
  get_incidence_reproduction_plot(hosp_adm_data,
                                  plot_main = scenario_label,
                                  scen_tag = scen_opt[i],
                                  scen_color = 4,
                                  data_incidence = data_incidence_scenario)
}

dev.off()


## POLYGONS MEDIA ----

# reproduction number => remove burn-in and last 2 weeks
#data_incidence_scenario$sec_cases[data_incidence_scenario$sim_date > max(data_incidence_scenario$sim_date)-14] <- NA
#data_incidence_scenario$sec_cases[data_incidence_scenario$sim_date < min(data_incidence_scenario$sim_date)+14] <- NA

add_copyright_statement <- function(y_pos = -200,bool_nl = TRUE){
  par(xpd=TRUE)
  copyr_statment <- ifelse(bool_nl,
                        'Gebaseerd op "Willem L. et al (2020, MedRxiv, under review)"\n Universiteit Antwerpen en UHasselt',
                        'Based on "Willem L. et al (2020, MedRxiv, under review)"\n University of Antwerp and UHasselt'
                        )
  text(max(pretty(data_incidence_scenario$sim_date)+15),
       y_pos,
       copyr_statment,
       cex=0.5,
       pos=2)
  par(xpd=FALSE)
  
}
cex_lgnd <- 0.55
date_threshold <- as.Date("2020-05-10")

pdf(smd_file_path(dir_results,paste0(output_tag,'_manuscript_polygon_bubble_media.pdf')),6,4)
library('RColorBrewer')

y_lim  <- c(0,700)
x_lim  <- range(data_incidence_scenario$sim_date)
plot_main <- ''
par(mar=c(5,5,0.8,0.8))
plot(hosp_adm_data$date,hosp_adm_data$num_adm,pch=20,
     col=0,
     xaxt='n', yaxt='n',
     xlab = '',ylab='COVID-19 hospitalisaties',
     ylim = y_lim,
     xlim = x_lim)
add_x_axis(data_incidence_scenario$sim_date,bool_num=T)
add_y_axis(y_lim)
sum_scen1  <- add_polygon('scen01','new_hospital_admissions',1,data_incidence_scenario)
sum_scen10 <- add_polygon('scen10','new_hospital_admissions',4,data_incidence_scenario[data_incidence_scenario$sim_date >= date_threshold,])
add_vertical_line("2020-05-10",TRUE)
legend('topright',
       c('Simulaties met open contact bubbels',
         'Simulaties met gesloten contact bubbels'),
       lwd=4,
       col=c(alpha(c(1,4),0.4)),
       cex=cex_lgnd,
       bg='white'
       )
add_copyright_statement()

# ADD REPORTED DATA
y_lim  <- c(0,700)
x_lim  <- range(data_incidence_scenario$sim_date)
plot_main <- ''
par(mar=c(5,5,0.8,0.8))
hosp_adm_data_fit <- hosp_adm_data[hosp_adm_data$date<= as.Date('2020-06-08'),]
hosp_adm_data_val <- hosp_adm_data[hosp_adm_data$date > as.Date('2020-06-08'),]

plot(hosp_adm_data_fit$date,hosp_adm_data_fit$num_adm,pch=20,
     col=1,
     xaxt='n', yaxt='n',
     xlab = '',ylab='COVID-19 hospitalisaties',
     ylim = y_lim,
     xlim = x_lim)

add_x_axis(data_incidence_scenario$sim_date,bool_num=T)
add_y_axis(y_lim)
sum_scen1  <- add_polygon('scen01','new_hospital_admissions',1,data_incidence_scenario)
sum_scen10 <- add_polygon('scen10','new_hospital_admissions',4,data_incidence_scenario[data_incidence_scenario$sim_date >= date_threshold,])
add_vertical_line("2020-05-10",TRUE)
legend('topright',
       c('Simulaties met open contact bubbels',
         'Simulaties met gesloten contact bubbels',
         'Gerapporteerd tot 08/06/2020'
         ),
       lty=c(1,1,1,NA),
       lwd=c(4,4,NA),
       pch=c(NA,NA,20,1),
       col=c(alpha(c(1,4),0.4),
             'darkslateblue',
             1),
       cex=cex_lgnd,
       bg='white'
)
add_copyright_statement()

# ADD REPORTED DATA (ENG)
y_lim  <- c(0,700)
x_lim  <- range(data_incidence_scenario$sim_date)
plot_main <- ''
par(mar=c(5,5,0.8,0.8))
hosp_adm_data_fit <- hosp_adm_data[hosp_adm_data$date<= as.Date('2020-06-08'),]
hosp_adm_data_val <- hosp_adm_data[hosp_adm_data$date > as.Date('2020-06-08'),]

plot(hosp_adm_data_fit$date,hosp_adm_data_fit$num_adm,pch=20,
     col=1,
     xaxt='n', yaxt='n',
     xlab = '',ylab='COVID-19 hospital admissions',
     ylim = y_lim,
     xlim = x_lim)

add_x_axis(data_incidence_scenario$sim_date)
add_y_axis(y_lim)
sum_scen1  <- add_polygon('scen01','new_hospital_admissions',1,data_incidence_scenario)
sum_scen10 <- add_polygon('scen10','new_hospital_admissions',4,data_incidence_scenario[data_incidence_scenario$sim_date >= date_threshold,])
add_vertical_line("2020-05-10",TRUE)
legend('topright',
       c('Simulations with open contact bubbles',
         'Simulations with closed contact bubbles',
         'Reported untill June 08, 2020'
       ),
       lty=c(1,1,1,NA),
       lwd=c(4,4,NA),
       pch=c(NA,NA,20,1),
       col=c(alpha(c(1,4),0.4),
             'darkslateblue',
             1),
       cex=cex_lgnd,
       bg='white'
)
add_copyright_statement(bool_nl=FALSE)

# FOCUS ON 'PREDICTION'
y_lim  <- c(0,700)
x_lim  <- range(data_incidence_scenario$sim_date)
plot_main <- ''
par(mar=c(5,5,0.8,0.8))
plot(hosp_adm_data_fit$date,hosp_adm_data_fit$num_adm,pch=20,
     col=1,
     xaxt='n', yaxt='n',
     xlab = '',ylab='COVID-19 Hospitalisaties',
     ylim = y_lim,
     xlim = x_lim)

add_x_axis(data_incidence_scenario$sim_date,bool_num=T)
add_y_axis(y_lim)
sum_scen1  <- add_polygon('scen01','new_hospital_admissions',1,data_incidence_scenario[data_incidence_scenario$sim_date>as.Date("2020-05-10"),])
sum_scen10 <- add_polygon('scen10','new_hospital_admissions',4,data_incidence_scenario[data_incidence_scenario$sim_date>as.Date("2020-05-10"),])
legend('topright',
       c('Voorspellingen met open contact bubbels',
         'Voorspellingen met gesloten contact bubbels',
         'Overlappende voorspellingen',
         'Gerapporteerd tot 08/06/2020'
       ),
       lty=c(1,1,1,NA),
       lwd=c(4,4,4,1),
       pch=c(NA,NA,NA,20,1),
       col=c(alpha(c(1,4),0.4),
             'darkslateblue',
             1),
       cex=cex_lgnd,
       bg='white'
)
add_vertical_line("2020-05-10",TRUE)
add_copyright_statement()

dev.off()

## POLYGONS MEDIA 2----
pdf(smd_file_path(dir_results,paste0(output_tag,'_manuscript_polygon_bubble_media2.pdf')),9,3)
par(mar=c(4,5,0.8,0.8))
par(mfrow=c(1,2))
## OPEN
plot(hosp_adm_data_fit$date,hosp_adm_data_fit$num_adm,pch=20,
     col=1,
     xaxt='n', yaxt='n',
     xlab = '',ylab='COVID-19 hospitalisaties',
     ylim = y_lim,
     xlim = x_lim)
add_x_axis(data_incidence_scenario$sim_date,bool_num=T)
add_y_axis(y_lim)
sum_scen1  <- add_polygon('scen01','new_hospital_admissions',1,data_incidence_scenario)
#sum_scen10 <- add_polygon('scen10','new_hospital_admissions',4,data_incidence_scenario)
legend('topright',
       c('Open contact bubbels',
         'Gerapporteerd tot 8 juni'),
       lwd=c(4,0),
       pch=c(NA,20),
       col=c(alpha(c(1),0.4),1),
       cex=cex_lgnd,
       bg='white'
)


#CLOSED
plot(hosp_adm_data_fit$date,hosp_adm_data_fit$num_adm,pch=20,
     col=1,
     xaxt='n', yaxt='n',
     xlab = '',ylab='COVID-19 hospitalisaties',
     ylim = y_lim,
     xlim = x_lim)
add_x_axis(data_incidence_scenario$sim_date,bool_num=T)
add_y_axis(y_lim)
add_vertical_line("2020-05-10",TRUE)
sum_scen1  <- add_polygon('scen01','new_hospital_admissions',1,data_incidence_scenario[data_incidence_scenario$sim_date < date_threshold,])
sum_scen10 <- add_polygon('scen10','new_hospital_admissions',4,data_incidence_scenario[data_incidence_scenario$sim_date >= date_threshold,])
legend('topright',
       c('Open contact bubbels',
         'Gesloten contact bubbels',
         'Gerapporteerd tot 8 juni'),
       lwd=c(4,4,NA),
       pch=c(NA,NA,20),
       col=c(alpha(c(1,4),0.4),1),
       cex=cex_lgnd,
       bg='white'
)
add_copyright_statement(y_pos = -250)

dev.off()


## BOXPLOTS ALL ----

total_hosp_range <- c(17,55)*1e3

# open pdf stream
pdf(smd_file_path(dir_results,paste0(output_tag,'_scenario_analysis_all.pdf')),5,5)

# daily counts... use all results per month
plot_montly_stats_vertical(data_incidence,'sec_cases',c(0.5,2),'Reproduction number')
plot_montly_stats_vertical(data_incidence,'new_hospital_admissions',NA,'Daily hospital admissions')

# cumulative counts, use max per experiment
bxplot_cumulative <- aggregate(cumulative_hospital_cases ~ scenario + scenario_label + col + exp_id + sim_month + reference_id + scenario_id, data = data_incidence,max)
plot_montly_stats_vertical(bxplot_cumulative,'cumulative_hospital_cases',total_hosp_range,'Total hospital admissions')

# close pdf stream
dev.off()

## BOXPLOTS ADULT SELECTION ----
# open pdf stream
pdf(smd_file_path(dir_results,paste0(output_tag,'_adult.pdf')),5,5)

# data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1:17),]
# data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1,10,12,14,16:17),]
data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1,10:17),]
# daily counts... use all results per month
plot_montly_stats_vertical(data_incidence_sel,'sec_cases',c(0.5,2),'Reproduction number')
plot_montly_stats_vertical(data_incidence_sel,'new_hospital_admissions',NA,'Daily hospital admissions')

# cumulative counts, use max per experiment
bxplot_cumulative <- aggregate(cumulative_hospital_cases ~ scenario  + scenario_label + col + exp_id + sim_month + reference_id + scenario_id, data = data_incidence_sel,max)
plot_montly_stats_vertical(bxplot_cumulative,'cumulative_hospital_cases',c(16,35)*1e3,'Total hospital admissions')

# close pdf stream
dev.off()

## BOXPLOTS ADULT SELECTION ----
# open pdf stream
pdf(smd_file_path(dir_results,paste0(output_tag,'_adult.pdf')),5,5)

# data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1:17),]
# data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1,10,12,14,16:17),]
data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1,10:17),]
# daily counts... use all results per month
plot_montly_stats_vertical(data_incidence_sel,'sec_cases',c(0.5,2),'Reproduction number')
plot_montly_stats_vertical(data_incidence_sel,'new_hospital_admissions',NA,'Daily hospital admissions')

# cumulative counts, use max per experiment
bxplot_cumulative <- aggregate(cumulative_hospital_cases ~ scenario  + scenario_label + col + exp_id + sim_month + reference_id + scenario_id, data = data_incidence_sel,max)
plot_montly_stats_vertical(bxplot_cumulative,'cumulative_hospital_cases',c(16,35)*1e3,'Total hospital admissions')

# close pdf stream
dev.off()

## BOXPLOTS ADULT CTS ----
# open pdf stream
pdf(smd_file_path(dir_results,paste0(output_tag,'_adult_cts.pdf')),5,5)

# data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1:17),]
data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1,17),]

# daily counts... use all results per month
plot_montly_stats_vertical(data_incidence_sel,'sec_cases',c(0.5,2),'Reproduction number')
plot_montly_stats_vertical(data_incidence_sel,'new_hospital_admissions',c(0,300),'Daily hospital admissions')

# cumulative counts, use max per experiment
bxplot_cumulative <- aggregate(cumulative_hospital_cases ~ scenario  + scenario_label + col + exp_id + sim_month + reference_id + scenario_id, data = data_incidence_sel,max)
plot_montly_stats_vertical(bxplot_cumulative,'cumulative_hospital_cases',c(16,35)*1e3,'Total hospital admissions')

# close pdf stream
dev.off()

## BOXPLOTS ADULT SCHOOL SELECTION ----
# open pdf stream
pdf(smd_file_path(dir_results,paste0(output_tag,'_adult_school.pdf')),5,5)

# data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1:17),]
data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(1:10,17),]

# daily counts... use all results per month
plot_montly_stats_vertical(data_incidence_sel,'sec_cases',c(0.5,2),'Reproduction number')
plot_montly_stats_vertical(data_incidence_sel,'new_hospital_admissions',NA,'Daily hospital admissions')

# cumulative counts, use max per experiment
bxplot_cumulative <- aggregate(cumulative_hospital_cases ~ scenario  + scenario_label + col + exp_id + sim_month + reference_id + scenario_id, data = data_incidence_sel,max)
plot_montly_stats_vertical(bxplot_cumulative,'cumulative_hospital_cases',c(16,120)*1e3,'Total hospital admissions')

# close pdf stream
dev.off()


## BOXPLOTS CHILD SELECTION ----
# open pdf stream
pdf(smd_file_path(dir_results,paste0(output_tag,'_child.pdf')),5,5)

data_incidence_sel <- data_incidence[data_incidence$scenario_id %in% c(21:37),]
# daily counts... use all results per month
plot_montly_stats_vertical(data_incidence_sel,'sec_cases',c(0.5,2),'Reproduction number')
plot_montly_stats_vertical(data_incidence_sel,'new_hospital_admissions',NA,'Daily hospital admissions')

# cumulative counts, use max per experiment
bxplot_cumulative <- aggregate(cumulative_hospital_cases ~ scenario  + scenario_label + col + exp_id + sim_month + reference_id + scenario_id, data = data_incidence_sel,max)
plot_montly_stats_vertical(bxplot_cumulative,'cumulative_hospital_cases',NA,'Total hospital admissions')

# close pdf stream
dev.off()


## BASELINE ADULT ----
project_dir_base     <- smd_file_path(dir_results,'20200615_175553_int1_baseline')
project_summary_base <- .rstride$load_project_summary(project_dir_base)
input_opt_base       <- .rstride$get_variable_model_param(project_summary_base)
data_incidence_base  <- data_incidence_scenario[grepl('scen01',data_incidence_scenario$scenario),]

# summary statistics
date_summary_start <- '2020-03-07'
date_summary_end   <- '2020-03-13'
num_exp            <- 10
print(project_dir_base)
print_transmission_summary(data_incidence_base,date_summary_start,date_summary_end,num_exp)

bool_fitting <- TRUE

# add config_id 
# get variable names of input_opt_design (fix if only one column)
if(ncol(input_opt_base) == 1) {
  project_summary_base$config_id  <- project_summary_base[,colnames(input_opt_base)]
  input_opt_base           <- data.frame(input_opt_base,config_id = c(input_opt_base))
} else{
  project_summary_base$config_id  <- apply(project_summary_base[,names(input_opt_base)],1,paste, collapse='_')
  input_opt_base$config_id <- apply(input_opt_base,1,paste, collapse='_')
}

# select period
if(bool_fitting) data_incidence_base <- data_incidence_base[data_incidence_base$sim_date < as.Date('2020-06-07'),]
data_incidence_base <- data_incidence_base[data_incidence_base$sim_date > as.Date('2020-02-27'),]

if(bool_fitting) {
  .rstride$create_pdf(project_dir_base,file_name = 'manuscript_fitting',6,4)
  } else{
    .rstride$create_pdf(project_dir_base,file_name = 'manuscript_scenario',6,4)
  }


# plot
plot_incidence_data(data_incidence_base,project_summary_base,
                    hosp_adm_data,input_opt_base,prevalence_ref,
                    bool_add_param = FALSE,
                    bool_only_hospital_adm = FALSE,
                    bool_add_axis4 = FALSE,
                    bool_seroprev_limited = TRUE) 

data_incidence_base$sec_cases[data_incidence_base$sim_date > as.Date('2020-08-15')] <- NA
if(bool_fitting) data_incidence_base$sec_cases[data_incidence_base$sim_date > as.Date('2020-04-25')] <- NA
plot(sec_cases ~ sim_date,data=data_incidence_base,type='l',
     col=alpha(1,0.5),
     ylim=c(0,4),
     xaxt='n',yaxt='n',
     xlab='Time of infection',
     ylab='Secondary cases\nReproduction number')
add_x_axis(data_incidence_base$sim_date)
add_y_axis(data_incidence_base$sec_cases)
add_breakpoints()

dev.off()




## BASELINE CHILD ----
project_dir_base     <- smd_file_path(dir_results,'20200615_175609_int21_child_base')
project_summary_base <- .rstride$load_project_summary(project_dir_base)
input_opt_base       <- .rstride$get_variable_model_param(project_summary_base)
data_incidence_base  <- data_incidence_scenario[grepl('scen21',data_incidence_scenario$scenario),]

# summary statistics
print(project_dir_base)
print_transmission_summary(data_incidence_base,date_summary_start,date_summary_end,num_exp)

bool_fitting <- TRUE

# add config_id 
# get variable names of input_opt_design (fix if only one column)
if(ncol(input_opt_base) == 1) {
  project_summary_base$config_id  <- project_summary_base[,colnames(input_opt_base)]
  input_opt_base           <- data.frame(input_opt_base,config_id = c(input_opt_base))
} else{
  project_summary_base$config_id  <- apply(project_summary_base[,names(input_opt_base)],1,paste, collapse='_')
  input_opt_base$config_id <- apply(input_opt_base,1,paste, collapse='_')
}

# select period
if(bool_fitting) data_incidence_base <- data_incidence_base[data_incidence_base$sim_date < as.Date('2020-06-07'),]
data_incidence_base <- data_incidence_base[data_incidence_base$sim_date > as.Date('2020-02-27'),]

if(bool_fitting) {
  .rstride$create_pdf(project_dir_base,file_name = 'manuscript_child_fitting',6,4)
} else{
  .rstride$create_pdf(project_dir_base,file_name = 'manuscript_schild_scenario',6,4)
}


# plot
plot_incidence_data(data_incidence_base,project_summary_base,
                    hosp_adm_data,input_opt_base,prevalence_ref,
                    bool_add_param = FALSE,
                    bool_only_hospital_adm = FALSE,
                    bool_add_axis4 = FALSE,
                    bool_seroprev_limited = TRUE) 

data_incidence_base$sec_cases[data_incidence_base$sim_date > as.Date('2020-08-15')] <- NA
if(bool_fitting) data_incidence_base$sec_cases[data_incidence_base$sim_date > as.Date('2020-04-25')] <- NA
plot(sec_cases ~ sim_date,data=data_incidence_base,type='l',
     col=alpha(1,0.5),
     ylim=c(0,4),
     xaxt='n',yaxt='n',
     xlab='Time of infection',
     ylab='Secondary cases\nReproduction number')
add_x_axis(data_incidence_base$sim_date)
add_y_axis(data_incidence_base$sec_cases)
add_breakpoints()

dev.off()



### RELATIVE DIFFERENCE ---- 
#par(mfrow=c(1,2))
flag_dates = data_incidence_scenario$sim_date > as.Date('2020-05-01')
flag_comp = data_incidence_scenario$scenario_id == 1 & flag_dates
flag_scen = data_incidence_scenario$scenario_id == 10 & flag_dates
names(data_incidence_scenario)

data_incidence$config_id

hosp_adm_comp <- aggregate(cumulative_hospital_cases ~ sim_date + config_id, data= data_incidence_scenario[flag_comp,],mean)
hosp_adm_scen <- aggregate(cumulative_hospital_cases ~ sim_date + config_id, data= data_incidence_scenario[flag_scen,],mean)

names(hosp_adm_comp)[3] <- 'comparator'
names(hosp_adm_scen)[3] <- 'scenario'

hosp_adm <- merge(hosp_adm_comp,hosp_adm_scen)

plot(hosp_adm$sim_date,
     hosp_adm$scenario / hosp_adm$comparator,
     ylim=c(0.5,1.5))
abline(h=1)
add_breakpoints()

table(hosp_adm$sim_date == max(hosp_adm$sim_date))
hosp_adm_sel <- hosp_adm[hosp_adm$sim_date == max(hosp_adm$sim_date),]
hosp_adm_sel$rel_diff <- hosp_adm_sel$scenario / hosp_adm_sel$comparator
hosp_adm_sel
boxplot(hosp_adm_sel$rel_diff)


#####
# cumulative mean per scenario and mixing_assumption

flag_date <- data_incidence_scenario$sim_date == max(data_incidence_scenario$sim_date)
hosp_adm_cum <- aggregate(cumulative_hospital_cases ~ scenario + sim_date + config_id + scenario_id, data= data_incidence_scenario[flag_date,],mean)

head(hosp_adm_cum)

opt_scenario
opt_config_id <- unique(hosp_adm_cum$config_id)
hosp_adm_rel  <- matrix(NA,ncol = length(opt_config_id),nrow=nrow(opt_scenario))
colnames(hosp_adm_rel) <- opt_config_id

i_scen =25
for(i_scen in 1:nrow(opt_scenario)){

  # select reference
  hosp_adm_ref <- hosp_adm_cum[hosp_adm_cum$scenario_id == opt_scenario$reference_id[i_scen],]
  names(hosp_adm_ref) <- paste0('ref_',names(hosp_adm_ref))
  
  # select scenario data
  hosp_adm_sel <- hosp_adm_cum[hosp_adm_cum$scenario_id == opt_scenario$scenario_id[i_scen],]
  
  # merge with reference data
  hosp_adm_sel <- merge(hosp_adm_ref,hosp_adm_sel,by.x=c('ref_sim_date','ref_config_id'),by.y = c('sim_date','config_id'),all.x = T)
  
  hosp_adm_sel$rel_hosp_admissions <- hosp_adm_sel$cumulative_hospital_cases / hosp_adm_sel$ref_cumulative_hospital_cases
  head(hosp_adm_sel)
  hosp_adm_rel[i_scen,hosp_adm_sel$ref_config_id] <- hosp_adm_sel$rel_hosp_admissions

}

dim(hosp_adm_rel)

hosp_adm_summary <- t(hosp_adm_rel)
colnames(hosp_adm_summary) <- opt_scenario$scenario_label
boxplot(hosp_adm_summary,las=2)
abline(h=1)


## OVERALL

plot_relative_hospital_adm <- function(data_incidence){
  flag_date    <- data_incidence$sim_date == max(data_incidence$sim_date)
  hosp_adm_cum <- aggregate(cumulative_hospital_cases ~ scenario + sim_date + scenario_id, data= data_incidence[flag_date,],mean)
  
  hosp_adm_cum <- merge(hosp_adm_cum,opt_scenario)
  hosp_adm_cum$relative_difference <- NA
  
  i_scen <- 1
  for(i_scen in 1:nrow(hosp_adm_cum)){
    
    hosp_scen <- hosp_adm_cum$cumulative_hospital_cases[i_scen]
    hosp_ref  <- hosp_adm_cum$cumulative_hospital_cases[hosp_adm_cum$scenario_id == hosp_adm_cum$reference_id[i_scen]]
    
    hosp_adm_cum$relative_difference[i_scen] <-  hosp_scen / hosp_ref
  }
  
  par(mar=c(10,4,2,2))
  bplt <- barplot(hosp_adm_cum$relative_difference,
                  col = hosp_adm_cum$col,
                  ylim=c(0,2),
                  yaxt='n',
                  ylab='Relative number of hospital admissions')
  
  y_ticks <- pretty(0:2,10)
  axis(2,y_ticks,las=2)
  abline(h=y_ticks,lty=3,col='gray')
  text(bplt,hosp_adm_cum$relative_difference,hosp_adm_cum$scenario_id,pos=3, xpd = TRUE)
  
  # x-axis
  text_x <- bplt
  text_y <- par("usr")[3] * ifelse(par("usr")[3]>0,0.95,1.3)
  print(text_y)
  text(text_x, text_y,
       srt = 60, adj = 1, xpd = TRUE,
       labels = hosp_adm_cum$scenario_label)
  
}

plot_relative_hospital_adm(data_incidence_scenario[data_incidence_scenario$scenario_id %in% 1:17,])
plot_relative_hospital_adm(data_incidence_scenario[data_incidence_scenario$scenario_id %in% 21:37,])





