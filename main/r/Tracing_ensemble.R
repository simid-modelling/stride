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
# CONTACT TRACING ENSEMBLE
#
############################################################################# #

#rm(list=ls())

source('./bin/rstride/rStride.R')

# set directory name with results and get output files
# dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200518_results'
# dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200531_results_cts/'
dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200608_results_cts/'


file_name_incidence <- dir(dir_results,pattern='incidence_processed.RData',recursive = T,full.names = T)

# output tag
output_tag <- paste0(format(Sys.Date(),'%Y%m%d'),'_results')

## LOAD DATA ----
i_file <- 1
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
          
          # add scenario name
          data_incidence_all$scenario <- scenario_name
          
          data_incidence_all$scenario_id <- as.numeric(substr(scenario_name,5,6))
          
          # check
          smd_print(i_file,scenario_name)
          smd_print(dim(data_incidence_all))
          
          # return
          data_incidence_all
        } -> data_incidence_scenario

# select delay of 21 days
unique(data_incidence_scenario$config_id)
data_incidence_scenario <- data_incidence_scenario[!grepl('_14',data_incidence_scenario$config_id),]
unique(data_incidence_scenario$config_id)
unique(data_incidence_scenario$tracing_id)
data_incidence_scenario$config_id <- gsub('_21','',data_incidence_scenario$config_id)
data_incidence_scenario$tracing_id <- gsub('_21','',data_incidence_scenario$tracing_id)


# make copy and add month
data_incidence <- data_incidence_scenario
data_incidence$sim_month <- format(data_incidence$sim_date,'%B')
table(data_incidence$scenario)


## CONFIG FILE ----
file_name_summary <- dir(dir_results,pattern='_summary.csv',recursive = T,full.names = T)

i_file <- 1
# load and aggregate results
foreach(i_file = 1:length(file_name_summary),
        .combine = rbind) %do% {
          
          # load results
          project_summary <- .rstride$load_project_summary(project_dir = dirname(file_name_summary[i_file]))

          # parse scenario name
          scenario_name <- substr(basename(file_name_incidence[i_file]),16,100)
          scenario_name <- gsub('_incidence_processed.RData','',scenario_name)
          scenario_name <- gsub('_int','scen',scenario_name)
          
          # fix single digit numbers
          if(grepl('scen._',scenario_name)){
            scenario_name <- gsub('scen','scen0',scenario_name)
          }
          project_summary$scenario <- scenario_name
          
          # return
          project_summary
        } -> project_summary_scenario

names(project_summary_scenario)

## AGGREGATE => AVERAGE
#cum_hosp_max <- aggregate(cumulative_hospital_cases ~ detection_probability + case_finding_efficency + test_false_negative + delay_contact_tracing + delay_testing + tracing_id + scenario_name, data_incidence, max)
cum_hosp_max <- aggregate(cumulative_hospital_cases ~ tracing_id + scenario, data_incidence[data_incidence$sim_date == max(data_incidence$sim_date),], mean)

## REFERENCE ----
flag_reference  <- data_incidence$scenario_id == 1 & data_incidence$contact_id == '40,30,-2000' & data_incidence$sim_date == max(data_incidence$sim_date)
#flag_reference  <- data_incidence$scenario_id == 1 & data_incidence$contact_id == '40,30,-1300' & data_incidence$sim_date == max(data_incidence$sim_date)
cum_hosp_no_cts <- aggregate(cumulative_hospital_cases ~ tracing_id + scenario, data_incidence[flag_reference,], mean)

## REFORMAT ----
flag_baseline_cts <- cum_hosp_max$tracing_id %in% c('0\\.5_0\\.7_0\\.1','3_1')
cum_hosp_max[flag_baseline_cts,]

plot(data_incidence$sim_date[flag_baseline_cts],
     data_incidence$new_hospital_admissions[flag_baseline_cts],type='p')

flag_baseline_cts <- data_incidence$tracing_id %in% c('5_1')
lines(data_incidence$sim_date[flag_baseline_cts],
     data_incidence$new_hospital_admissions[flag_baseline_cts])

## PLOT FUNCTION: MATRIX FORMAT ----
plot_cts_matrix <- function(cts_matrix,row_lab,col_lab,row_values,col_values,legend_label){
        
        cts_matrix[cts_matrix>1] <- 1
        
        redc <- rev(colorspace::sequential_hcl(50))
        
        par(mar=c(5, 6, 2, 7),mgp=c(3,0.5,0))
        p <- simage(s = cts_matrix, 
                    xlab=row_lab,
                    ylab=col_lab, 
                    legend.width=1.5,
                    #legend.lab = legend_label,
                    legend.mar = 6,
                   #legend.shrink = 0.8,
                    #cex.lab = 2,
                    #slim=c(min(cts_matrix,na.rm=T), 1), 
                    slim=c(0.6,1), 
                    cex.lab=1,
                    cex.main=1.2, 
                    las=0.1,
                    xaxt="n",
                    yaxt="n",
                    col=redc)
                   
        # set x-axis 
        plt_xticks <- seq(0,1,length=nrow(cts_matrix))
        axis(1, at=plt_xticks, labels = row_values,cex.axis=1.0,tick = FALSE)
        
        # set y-axis 
        plt_yticks <- seq(0,1,length=ncol(cts_matrix))        
        axis(2, at=plt_yticks, labels = col_values,cex.axis=1.0,tick = FALSE,las=1)
        
        # set legend label(with adjusted size)
        mtext(legend_label,side=4,cex=0.5,padj=13)
}


## OPEN PDF STREAM ----
pdf(smd_file_path(dir_results,paste0(output_tag,'_manuscript_CTS.pdf')),10,3)
par(mar=c(5,5,2,1),mfrow=c(1,3))
#par(mar=c(4,4,1,2))


## GENERAL ----
flag_scen <- grepl('scen18',cum_hosp_max$scenario)

cum_hosp_scen <- cum_hosp_max[flag_scen,]
xx <- matrix(as.numeric(unlist(strsplit(cum_hosp_scen$tracing_id,'_'))),ncol=3,byrow=T)
colnames(xx) <- c('detection_probability','case_finding_efficency','test_false_negative')
cum_hosp_scen <- cbind(cum_hosp_scen,xx)
 
# cum_hosp_scen$relative_hospital_admissions <- cum_hosp_scen$cumulative_hospital_cases / cum_hosp_no_cts$cumulative_hospital_cases
# 
# bxplt <- boxplot(relative_hospital_admissions ~  detection_probability , data=cum_hosp_scen,las=1,
#         ylab= 'Relative number hospital admissions', yaxt='n',xaxt='n',
#         xlab = 'CTS index cases / symptomatic cases')
# axis(1,seq(1,length(bxplt$names),1),bxplt$names)
# add_y_axis(cum_hosp_scen$relative_hospital_admissions)

############################################################################@ #
## TESTING VS TRACING
cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$test_false_negative == 0.1,]
opt_case_finding_efficency <- unique(cum_hosp_sel$case_finding_efficency)*100
num_row <- length(opt_case_finding_efficency)

opt_detection_probability    <- unique(cum_hosp_sel$detection_probability)*100
num_col    <- length(opt_detection_probability) 

cts_matrix <- matrix(cum_hosp_sel$cumulative_hospital_cases,
                     nrow=num_row,ncol=num_col,byrow=T)

## RELATIVE
plot_cts_matrix(cts_matrix / cum_hosp_no_cts$cumulative_hospital_cases,
                col_lab="Contacts traced and tested",
                row_lab='CTS index cases / symptomatic cases',
                col_values = paste0(opt_case_finding_efficency,'%'),
                row_values = paste0(opt_detection_probability,'%'),
                legend_label='Relative hospital admissions')
points(2/3,2/3,pch=4)

####################################################### #
## TRACING VS FALSE NEGATIVE TEST RATE
# cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$detection_probability == 0.5,]
# opt_case_finding_efficency <- unique(cum_hosp_sel$case_finding_efficency)*100
# num_row <- length(opt_case_finding_efficency)
cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$case_finding_efficency == 0.7,]
opt_test_false_negative    <- unique(cum_hosp_sel$test_false_negative)*100
num_row    <- length(opt_test_false_negative) 

opt_detection_probability    <- unique(cum_hosp_sel$detection_probability)*100
num_col    <- length(opt_detection_probability) 


cts_matrix <- t(matrix(cum_hosp_sel$cumulative_hospital_cases,
                     nrow=num_row,ncol=num_col,byrow=F))
head(cum_hosp_sel)
## RELATIVE
plot_cts_matrix(cts_matrix / cum_hosp_no_cts$cumulative_hospital_cases,
                col_lab="False negative rate",
                row_lab='CTS index cases / symptomatic cases',
                col_values = paste0(opt_test_false_negative,'%'),
                row_values = paste0(opt_detection_probability,'%'),
                legend_label='Relative hospital admissions')
points(2/3,0,pch=4)

##################################################################################### #
## DELAY ----

flag_scen <- grepl('scen19',cum_hosp_max$scenario)

cum_hosp_sel <- cum_hosp_max[flag_scen,]
xx <- matrix(as.numeric(unlist(strsplit(cum_hosp_sel$tracing_id,'_'))),ncol=2,byrow=T)
colnames(xx) <- c('delay_contact_tracing','delay_testing')
cum_hosp_scen <- cbind(cum_hosp_sel,xx)

opt_delay_contact_tracing <- unique(cum_hosp_scen$delay_contact_tracing)
num_row <- length(opt_delay_contact_tracing)

opt_delay_testing    <- unique(cum_hosp_scen$delay_testing)
num_col              <- length(opt_delay_testing) 

head(cum_hosp_scen)
cts_matrix <- matrix(cum_hosp_scen$cumulative_hospital_cases,
                     nrow=num_row,ncol=num_col,byrow=T)
# colnames(cts_matrix) <- paste0('delay_tracing',1:5)
# rownames(cts_matrix) <- paste0('delay_testing',1:5)
plot_cts_matrix(cts_matrix / cum_hosp_no_cts$cumulative_hospital_cases,
                col_lab="Delay test index case",
                row_lab="Delay contact tracing",
                col_values = paste0(opt_delay_testing),
                row_values = paste0(opt_delay_contact_tracing),
                legend_label='Relative hospital admissions'
                )
points(0,2/5,pch=4)

# close pdf stream
dev.off()


##### MONTLY AVERAGE ####
## AGGREGATE => AVERAGE

flag_date <- data_incidence$sim_date %in% (as.Date('2020-08-01'):as.Date('2020-08-31'))
cum_hosp_max <- aggregate(new_hospital_admissions ~ tracing_id + scenario, data_incidence[flag_date,], mean)
names(data_incidence)

## REFERENCE ----
flag_reference  <- data_incidence$scenario_id == 1 & data_incidence$contact_id == '40,30,-2000' & data_incidence$sim_date == max(data_incidence$sim_date)
#flag_reference  <- data_incidence$scenario_id == 1 & data_incidence$contact_id == '40,30,-1300' & data_incidence$sim_date == max(data_incidence$sim_date)
#cum_hosp_no_cts <- aggregate(cumulative_hospital_cases ~ tracing_id + scenario, data_incidence[flag_reference,], mean)
cum_hosp_no_cts <- aggregate(new_hospital_admissions ~ tracing_id + scenario, data_incidence[flag_reference,], mean)
cum_hosp_no_cts

## PLOT FUNCTION: MATRIX FORMAT ----
plot_cts_matrix_avg <- function(cts_matrix,row_lab,col_lab,row_values,col_values,legend_label){
        
        cts_matrix[cts_matrix>1] <- 1
        
        redc <- rev(colorspace::sequential_hcl(40))
        
        par(mar=c(5, 6, 2, 7),mgp=c(3,0.5,0))
        p <- simage(s = cts_matrix, 
                    xlab=row_lab,
                    ylab=col_lab, 
                    legend.width=1.5,
                    #legend.lab = legend_label,
                    legend.mar = 6,
                    #legend.shrink = 0.8,
                    #cex.lab = 2,
                    #slim=c(min(cts_matrix,na.rm=T), 1), 
                    #slim=c(0,1), 
                    cex.lab=1,
                    cex.main=1.2, 
                    las=0.1,
                    xaxt="n",
                    yaxt="n",
                    col=redc)
        
        # set x-axis 
        plt_xticks <- seq(0,1,length=nrow(cts_matrix))
        axis(1, at=plt_xticks, labels = row_values,cex.axis=1.0,tick = FALSE)
        
        # set y-axis 
        plt_yticks <- seq(0,1,length=ncol(cts_matrix))        
        axis(2, at=plt_yticks, labels = col_values,cex.axis=1.0,tick = FALSE,las=1)
        
        # set legend label(with adjusted size)
        mtext(legend_label,side=4,cex=0.5,padj=13)
}


## OPEN PDF STREAM ----
pdf(smd_file_path(dir_results,paste0(output_tag,'_manuscript_CTS_august.pdf')),10,3)
par(mar=c(5,5,2,1),mfrow=c(1,3))
#par(mar=c(4,4,1,2))


## GENERAL ----
flag_scen <- grepl('scen18',cum_hosp_max$scenario)

cum_hosp_scen <- cum_hosp_max[flag_scen,]
xx <- matrix(as.numeric(unlist(strsplit(cum_hosp_scen$tracing_id,'_'))),ncol=3,byrow=T)
colnames(xx) <- c('detection_probability','case_finding_efficency','test_false_negative')
cum_hosp_scen <- cbind(cum_hosp_scen,xx)

# cum_hosp_scen$relative_hospital_admissions <- cum_hosp_scen$cumulative_hospital_cases / cum_hosp_no_cts$cumulative_hospital_cases
# 
# bxplt <- boxplot(relative_hospital_admissions ~  detection_probability , data=cum_hosp_scen,las=1,
#         ylab= 'Relative number hospital admissions', yaxt='n',xaxt='n',
#         xlab = 'CTS index cases / symptomatic cases')
# axis(1,seq(1,length(bxplt$names),1),bxplt$names)
# add_y_axis(cum_hosp_scen$relative_hospital_admissions)

############################################################################@ #
## TESTING VS TRACING
cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$test_false_negative == 0.1,]
opt_case_finding_efficency <- unique(cum_hosp_sel$case_finding_efficency)*100
num_row <- length(opt_case_finding_efficency)

opt_detection_probability    <- unique(cum_hosp_sel$detection_probability)*100
num_col    <- length(opt_detection_probability) 

cts_matrix <- matrix(cum_hosp_sel$new_hospital_admissions,
                     nrow=num_row,ncol=num_col,byrow=T)

## RELATIVE
plot_cts_matrix_avg(cts_matrix / cum_hosp_no_cts$new_hospital_admissions,
                col_lab="Contacts traced and tested",
                row_lab='CTS index cases / symptomatic cases',
                col_values = paste0(opt_case_finding_efficency,'%'),
                row_values = paste0(opt_detection_probability,'%'),
                legend_label='Monthly average, daily hospital admissions')
points(2/3,2/3,pch=4)

# ####################################################### #
# ## TRACING VS FALSE NEGATIVE TEST RATE
# # cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$detection_probability == 0.5,]
# # opt_case_finding_efficency <- unique(cum_hosp_sel$case_finding_efficency)*100
# # num_row <- length(opt_case_finding_efficency)
# cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$case_finding_efficency == 0.7,]
# opt_test_false_negative    <- unique(cum_hosp_sel$test_false_negative)*100
# num_row    <- length(opt_test_false_negative) 
# 
# opt_detection_probability    <- unique(cum_hosp_sel$detection_probability)*100
# num_col    <- length(opt_detection_probability) 
# 
# 
# cts_matrix <- t(matrix(cum_hosp_sel$new_hospital_admissions,
#                        nrow=num_row,ncol=num_col,byrow=F))
# head(cum_hosp_sel)
# ## RELATIVE
# plot_cts_matrix_avg(cts_matrix / cum_hosp_no_cts$new_hospital_admissions,
#                 col_lab="False negative rate",
#                 row_lab='CTS index cases / symptomatic cases',
#                 col_values = paste0(opt_test_false_negative,'%'),
#                 row_values = paste0(opt_detection_probability,'%'),
#                 legend_label='Relative hospital admissions')
# points(2/3,0,pch=4)

####################################################### #
## TRACING VS FALSE NEGATIVE TEST RATE
# cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$detection_probability == 0.5,]
# opt_case_finding_efficency <- unique(cum_hosp_sel$case_finding_efficency)*100
# num_row <- length(opt_case_finding_efficency)
cum_hosp_sel <- cum_hosp_scen[cum_hosp_scen$detection_probability == 0.5,]
opt_test_false_negative    <- unique(cum_hosp_sel$test_false_negative)*100
num_row    <- length(opt_test_false_negative) 

opt_detection_probability    <- unique(cum_hosp_sel$case_finding_efficency)*100
num_col    <- length(opt_detection_probability) 


cts_matrix <- t(matrix(cum_hosp_sel$new_hospital_admissions,
                       nrow=num_row,ncol=num_col,byrow=F))
head(cum_hosp_sel)
## RELATIVE
plot_cts_matrix_avg(t(cts_matrix / cum_hosp_no_cts$new_hospital_admissions),
                    row_lab="False negative rate",
                    col_lab='Contacts traced and tested',
                    row_values = paste0(opt_test_false_negative,'%'),
                    col_values = paste0(opt_detection_probability,'%'),
                    legend_label='Relative hospital admissions')
points(0,2/3,pch=4)


##################################################################################### #
## DELAY ----

flag_scen <- grepl('scen19',cum_hosp_max$scenario)

cum_hosp_sel <- cum_hosp_max[flag_scen,]
xx <- matrix(as.numeric(unlist(strsplit(cum_hosp_sel$tracing_id,'_'))),ncol=2,byrow=T)
colnames(xx) <- c('delay_contact_tracing','delay_testing')
cum_hosp_scen <- cbind(cum_hosp_sel,xx)

opt_delay_contact_tracing <- unique(cum_hosp_scen$delay_contact_tracing)
num_row <- length(opt_delay_contact_tracing)

opt_delay_testing    <- unique(cum_hosp_scen$delay_testing)
num_col              <- length(opt_delay_testing) 

head(cum_hosp_scen)
cts_matrix <- matrix(cum_hosp_scen$new_hospital_admissions,
                     nrow=num_row,ncol=num_col,byrow=T)
# colnames(cts_matrix) <- paste0('delay_tracing',1:5)
# rownames(cts_matrix) <- paste0('delay_testing',1:5)
plot_cts_matrix_avg(cts_matrix / cum_hosp_no_cts$new_hospital_admissions,
                col_lab="Delay test index case",
                row_lab="Delay contact tracing",
                col_values = paste0(opt_delay_testing),
                row_values = paste0(opt_delay_contact_tracing),
                legend_label='Relative hospital admissions'
)
points(0,2/5,pch=4)

# close pdf stream
dev.off()






