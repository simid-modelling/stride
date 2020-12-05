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
  
  # if any download failed => use defaults
  if(any(is.na(c(hosp_ref_file,cases_ref_file,tests_ref_file)))){
    return(read.table(backup_file,sep=',',header=T))
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
  ref_test_data     <- aggregate(TESTS_ALL ~ DATE, data= ref_test_data_all,sum,na.rm=T)
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
  exit_status     <- tryCatch(download.file(cases_ref_url,case_ref_file,quiet=TRUE),
                          error = function(e){return(-1)})
  return(ifelse(exit_status==0,case_ref_file,NA))
}

load_observed_seroprevalence_data <- function(collection_period = 0:2,
                                              analysis = "overall")
{
  
  ## SERO-PREVALENCE DATA ----
  prevalence_ref <- read.table('./data/covid19_serology_BE_reference.csv',sep=',',header=T)
  
  # reformat
  prevalence_ref$collection_date_start <- as.Date(prevalence_ref$collection_date_start,format='%d/%m/%Y')
  prevalence_ref$collection_date_end   <- as.Date(prevalence_ref$collection_date_end,format='%d/%m/%Y')
  prevalence_ref$collection_days       <- prevalence_ref$collection_date_end - prevalence_ref$collection_date_start
  prevalence_ref$seroprevalence_date   <- prevalence_ref$collection_date_start - prevalence_ref$days_seroconversion + (prevalence_ref$collection_days/2)
  
  # calculate total incidence (age-specific demography is taken into account later)
  # pop_size_be <- 11e6
  # prevalence_ref$point_incidence_mean  <- prevalence_ref$seroprevalence_weighted * pop_size_be
  # prevalence_ref$point_incidence_low   <- prevalence_ref$seroprevalence_2p5  * pop_size_be
  # prevalence_ref$point_incidence_high  <- prevalence_ref$seroprevalence_97p5 * pop_size_be
  prevalence_ref$point_incidence_mean  <- NA  # add columns
  prevalence_ref$point_incidence_low   <- NA  # add columns
  prevalence_ref$point_incidence_high  <- NA  # add columns

  
  # NEW get age-specific demography data
  popdata <- get_population_data('belgium',2020,unique(prevalence_ref$age_min))
  prevalence_pop <- merge(prevalence_ref,popdata,by='age_min')

  # copy the (Stride-based) total popsize for non-age-specific analysis
  pop_size_be <- 11e6                        # stride population
  # pop_size_be <- sum(popdata$population)   # actual population
  prevalence_pop$population[prevalence_pop$analysis != 'age'] <- pop_size_be
  
  prevalence_pop$point_incidence_mean  <- prevalence_pop$seroprevalence_weighted * prevalence_pop$population
  prevalence_pop$point_incidence_low   <- prevalence_pop$seroprevalence_2p5  * prevalence_pop$population
  prevalence_pop$point_incidence_high  <- prevalence_pop$seroprevalence_97p5 * prevalence_pop$population
  
  # select columns
  prevalence_ref <- prevalence_pop[,names(prevalence_ref)]
  
  # select 'X' sample rounds
  prevalence_ref <- prevalence_ref[prevalence_ref$collection_period %in% collection_period &
                                   prevalence_ref$analysis %in% analysis,]

  prevalence_ref
  
  # return
  return(prevalence_ref)
  
}

# Hospital survey
# Faes et al 2020 ()
load_hospital_surge_survey_data <- function(){
  
  # average by age for week 11 - 13
  hosp_adm <- c(1.42034063,    # 00-09
                0.230950038,   # 10-19
                1.805459467,   # ...
                3.855632946,
                9.641311491,
                15.22273271,
                18.45155261,
                22.6793626 +  20.5921284 + 6.100529114) # +70
  
  hosp_delay <- c(rep(3,2),  # 0-19 
                  rep(7,4),  # 20-59
                  rep(6,2))#  # +70
                  #rep(1,1))  # +80
  
  out_hosp <- data.frame(age_break_min = seq(0,70,10),
                         admissions_relative = hosp_adm/100, 
                         delay = hosp_delay)           
  
  return(out_hosp)
}



get_population_data <- function(country,year,age_breaks=NA){
  
  popdata_agecat <- wpp_age('belgium',2020)

  agecat_size <- unique(diff(sort(popdata_agecat$lower.age.limit)))

  # add final age group
  popdata_age <- approx(x = popdata_agecat$lower.age.limit,
                        y = popdata_agecat$population / agecat_size,
                        # method = 'linear',
                        method = 'constant',
                        
                        xout = seq(min(popdata_agecat$lower.age.limit),
                                 max(popdata_agecat$lower.age.limit+agecat_size-1),
                                 1),
                        rule=2
                        )

  pop_out  <- data.frame(country     = country,
                         year        = year,
                         age_min     = popdata_age$x,
                         population  = popdata_age$y)
  

  # if no age breaks given, use one year age groups  
  if(any(is.na(age_breaks))){
    age_breaks <- pop_out$age_min
  }
  
  # cut ages
  pop_out$age_cat <- cut(pop_out$age_min,
                         breaks=unique(c(age_breaks,max(pop_out$age_min+1))),
                         include.lowest = T,right=F)
  
  # aggregate by age group
  pop_out <- aggregate(. ~ country + year + age_cat, data = pop_out, sum)
  
  # add minimum age per group
  pop_out$age_min <- age_breaks
  
  # return result
  return(pop_out)
  
}

