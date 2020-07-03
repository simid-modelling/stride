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
# PROVIDE PUBLIC HEALTH AGENCY DATA                                        ##
#############################################################################

get_observed_incidence_data <- function(num_samples = 1)
{
  
  ## REFERENCE DATA COVID-19: new hospital admissions ----
  # use (local version of) most recent SCIENSANO data (or local backup version)
  ref_data_file_name <- smd_file_path('data',paste0('covid19_reference_data_',gsub('-','',Sys.Date()),'.csv'))
  backup_file        <- smd_file_path('data',paste0('covid19_reference_data.csv'))
  
  # if present: return data
  if(file.exists(ref_data_file_name)){
    return(read.table(ref_data_file_name,sep=',',header=T))
  }
  
  # download files
  hosp_ref_file  <- download_ref_file('https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv')
  cases_ref_file <- download_ref_file('https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv')
  tests_ref_file <- download_ref_file('https://epistat.sciensano.be/Data/COVID19BE_tests.csv')
  
  # if any download gave NA => use defaults
  if(any(is.na(c(hosp_ref_file,cases_ref_file,tests_ref_file)))){
    return(read.table(ref_data_file_name,sep=',',header=T))
  }

  # load reference hospital data
  ref_hosp_data_all     <- read.table(hosp_ref_file,sep=',',header = T)
  hosp_adm_data         <- aggregate(. ~ DATE, data= ref_hosp_data_all[,c('NEW_IN','TOTAL_IN','DATE')],sum,na.rm=T)
  names(hosp_adm_data)  <- c('sim_date','hospital_admissions','hospital_load')
  hosp_adm_data$cumulative_hospital_admissions <- cumsum(hosp_adm_data$hospital_admissions)
  head(hosp_adm_data)
  
  # load reference case data
  ref_case_data_all     <- read.table(cases_ref_file,sep=',',header = T)
  ref_case_data_table <- data.table(ref_case_data_all[!is.na(ref_case_data_all$AGEGROUP) & !is.na(ref_case_data_all$DATE),])
  ref_case_data_table[,ID := 1:nrow(ref_case_data_table),]
  ref_case_data_age        <- data.frame(get_summary_table(ref_case_data_table,'DATE','AGEGROUP','cases'))
  names(ref_case_data_age) <- gsub('\\.','_',names(ref_case_data_age))
  head(ref_case_data_age)
  
  # load test data
  ref_test_data_all <- read.table(tests_ref_file,sep=',',header = T)
  ref_test_data     <- aggregate(TESTS ~ DATE, data= ref_test_data_all,sum,na.rm=T)
  names(ref_test_data)  <- c('sim_date','covid19_tests')
  
  # merge hospital and case data
  ref_data <- merge(hosp_adm_data,ref_case_data_age,all=TRUE)
  ref_data <- merge(ref_data,ref_test_data,all = TRUE)
  names(ref_data)
  
  # save as csv
  write.table(ref_data,file=ref_data_file_name,sep=',',row.names=F)
  
  # return data
  return(ref_data)
}


# download file from URL
# if connection is not possible: return NA
download_ref_file <- function(cases_ref_url,data_dir = 'data'){
  case_ref_file   <- file.path(data_dir,basename(cases_ref_url))
  tryCatch(download.file(cases_ref_url,case_ref_file,quiet=TRUE),
           error = function(e){return(NA)})
  return(case_ref_file)
}


