############################################################################ #
#  This file is part of the Stride software. 
#
#  Copyright 2020, Willem L, Libin P
############################################################################ #
#
# TO CREATE CALENDAR FILE(S) FOR UNIVERSAL TESTING SIMULATIONS
#
# CONTAINING:
# 1. Public and school holidays
# 2. Contact reductions (covid19)
#       * pre-, primairy and secondary school
#       * workplace
#       * community
#       * household clusters     ==>> NOT USED HERE
# 3. Imported cases (covid19)
# 4. Contact tracing (covid19)   ==>> NOT USED HERE
# 5. Universal testing (covid19)
#
# GOAL(S): 
# - to switch from lockdown to universal testing
# - what happens if cases are imported when local transmission is controlled  
#
# NOTES: 
# - In this script, all variables with a name starting with "dcal_" are 
#   automatically merged.
# - Creates one csv calendar file with and one without imported cases
#
############################################################################ #
## code for debugging ----
if(0==1){
  
  # specify delay to import cases
  delay_import_cases <- 8*7
  
  # create calender with switch on the 1th of May
  date_policy_switch <- "2020-05-01"
  create_calenders_universal_testing(date_policy_switch,delay_import_cases)
  
  # create calender with switch on the 1th of July
  create_calenders_universal_testing("2020-07-01",delay_import_cases)
  
}

create_calenders_universal_testing <- function(date_policy_switch, delay_import_cases)
{
  
  ########################################### #
  ## INITIATE DATA                       ####
  ########################################### #
  
  ## default value in C++ CALENDAR vectors ==>> 0
  
  ## make sure the given date to switch policy is of type "date"
  date_policy_switch <- as.Date(date_policy_switch)
  
  ## set lockdown period
  dates_lockdown <- seq(as.Date('2020-03-14'),date_policy_switch-1,1)
  
  ## set lockdown period
  dates_universal_testing <- seq(date_policy_switch,as.Date('2021-12-31'),1) # use a dummy end (has no impact on the simulation)
  
  ## set dates for imported cases
  dates_import_cases <- dates_universal_testing + delay_import_cases

  ########################################### #
  ## 1.a Public holidays                 ####
  ########################################### #
  
  data.table(category = "general",
             date     = as.Date(c('2020-01-01','2020-04-13','2020-05-01','2020-05-21','2020-06-01', # 2020
                                  '2020-07-21','2020-08-15','2020-11-01','2020-11-11','2020-12-25')),
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F) -> d_calendar_holiday
  
  summary(d_calendar_holiday)
  
  ########################################### #
  ## 1.b School holidays                 ####
  ########################################### #
  
  data.table(category = "school_holiday",
             date     = c(seq(as.Date('2020-01-01'),as.Date('2020-01-05'),1), # 2020
                          seq(as.Date('2020-02-24'),as.Date('2020-02-29'),1),
                          seq(as.Date('2020-04-06'),as.Date('2020-04-19'),1),
                          seq(as.Date('2020-07-01'),as.Date('2020-08-31'),1),
                          seq(as.Date('2020-11-02'),as.Date('2020-11-08'),1),
                          seq(as.Date('2020-12-21'),as.Date('2020-12-31'),1)),
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F
  ) -> d_school_holidays
  
  data.table(category = "college",
             date     = c(seq(as.Date('2020-01-01'),as.Date('2020-01-05'),1), # 2020
                          seq(as.Date('2020-02-24'),as.Date('2020-02-29'),1),
                          seq(as.Date('2020-04-06'),as.Date('2020-04-19'),1),
                          seq(as.Date('2020-07-01'),as.Date('2020-09-20'),1),# summer break untill September, 20
                          #seq(as.Date('2020-11-02'),as.Date('2020-11-08'),1), # no fall break
                          seq(as.Date('2020-12-21'),as.Date('2020-12-31'),1)),
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F
  ) -> d_college_holidays
  
  d_calendar_holiday <- rbind(d_calendar_holiday,d_school_holidays[,category := 'preschool'])
  d_calendar_holiday <- rbind(d_calendar_holiday,d_school_holidays[,category := 'primary_school'])
  d_calendar_holiday <- rbind(d_calendar_holiday,d_school_holidays[,category := 'secondary_school'])
  d_calendar_holiday <- rbind(d_calendar_holiday,d_college_holidays)
  
  ########################################################### #
  ##  2a. Contact reductions: school closures              ####
  ########################################################### #
  #       * (pre-, primary and secondary school)
  
  # set default school closure
  data.table(category = "school_closure",
             date     = dates_lockdown,
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F
  ) -> d_school_closure
  
  # preschool
  dcal_preschool_closure <- copy(d_school_closure)
  dcal_preschool_closure[,category := 'preschool']
  
  # primary school
  dcal_primary_closure <- copy(d_school_closure)
  dcal_primary_closure[,category := 'primary_school']
  
  #secondary school
  dcal_secondary_closure <- copy(d_school_closure)
  dcal_secondary_closure[,category := 'secondary_school']
  
  # college (default remains closed)
  data.table(category = "college",
             date     = c(dates_lockdown,dates_universal_testing),
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F
  ) -> dcal_college_closure

  
  ########################################### #
  ##  2b. Contact reductions: other        ####
  ########################################### #
  #       * workplace
  #       * community
  #       * household clusters (not included here)
  
  # workplace distancing
  data.table(category = "workplace_distancing",
             date     = dates_lockdown,
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F
  ) -> dcal_workplace_distancing
  
  
  # workplace distancing
  data.table(category = "community_distancing",
             date     = dates_lockdown,
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F
  ) -> dcal_community_distancing
  
  
  ########################################### #
  ## 3. Imported cases                 ####
  ########################################### #
  
  data.table(category = "imported_cases",
               date     = dates_import_cases,
               value    = 1,
               type = 'boolean',
               stringsAsFactors = F
  ) -> dcal_imported_cases
  
  
  ########################################### #
  ##  4. Contact tracing                 ####
  ########################################### #
  
  ########################################### #
  ## 5. Universal testing                 ####
  ########################################### #
  
  data.table(category = "universal_testing",
             date     = dates_universal_testing,
             value    = 1,
             type = 'boolean',
             stringsAsFactors = F
  ) -> dcal_universal_testing
  
  
  ########################################### #
  ## MERGE HOLIDAYS & OTHER CALENDAR ITEMS ####
  ########################################### #
  
  ## make sure, no holidays are present during "universal testing"
  d_calendar_holiday <- d_calendar_holiday[!d_calendar_holiday$date %in% dates_universal_testing,]
  
  # get 'dcal_*' variables
  opt_other <- ls(pattern='dcal_')
  
  # combine all 'dcal_*' variable
  d_calendar_all <- foreach(i_other  = opt_other,
                      .init    = d_calendar_holiday,
                      .combine = 'rbind') %do% {
                        get(i_other)
                      } 
  unique(d_calendar_all$category)
  
  ########################################### #
  ## EXPLORE DATA                         ####
  ########################################### #
  
  # open pdf stream
  #pdf(file='./sim_output/calendar_profile.pdf',6,6)
  
  plot_calendar <- function(dt_calendar){
    category_opt <- unique(dt_calendar$category)
    par(mfrow=c(4,2))
    
    x_lim <- range(dt_calendar$date)
    i_cat <- category_opt[1]
    for(i_cat in category_opt){
      plot(x   = dt_calendar[category == i_cat,date],
           y   = dt_calendar[category == i_cat,value],
           xlim = x_lim,
           ylim = range(0,1,dt_calendar$value),
           col  = 1,
           pch  = 15,
           main = i_cat,
           bty='n',
           xlab = '',
           ylab = unique(dt_calendar[,type])
           #xaxt = 'n'
      )
      #add_x_axis(x_lim,num_ticks = 12,bool_numeric = T)      
    }
  }
  plot_calendar(d_calendar_all)
  
  # close pdf stream
  #dev.off()
  
  ########################################### #
  ## SAVE AS XML AND CSV	 	         ####
  ########################################### #
  
  # get "day of the year" the policy switch takes place
  day_index_policy_switch  <- strftime(date_policy_switch, format = "%j")
  
  # get "day of the year" the seeding of infected cases (re)starts
  day_index_import_cases   <- strftime(min(dates_import_cases), format = "%j")
  
  # create standardised names
  filename_calendar        <- paste0('calendar_belgium_covid19_universaltest_d',day_index_policy_switch,'.csv')
  filename_calendar_import <- gsub('.csv',paste0('_import_d',day_index_import_cases,'.csv'),filename_calendar)
  filename_calendar_import_college <- gsub('.csv',paste0('_import_college_d',day_index_import_cases,'.csv'),filename_calendar)
  
  # save all calendar info as csv
  write.table(d_calendar_all,
              file = smd_file_path('data',filename_calendar_import),
              sep=',',row.names=F,quote=F)
  
  # save subset (no imported cases) as csv
  write.table(d_calendar_all[!category %in% c('import_cases'),],
              file = smd_file_path('data',filename_calendar),
              sep=',',row.names=F,quote=F)

  # save subset (college reopens)
  d_calendar_all[date %in% dates_universal_testing & category == "college",value:=0]
  write.table(d_calendar_all,
              file = smd_file_path('data',filename_calendar_import_college),
              sep=',',row.names=F,quote=F)
  plot_calendar(d_calendar_all)
  
  smd_print("CREATED", smd_file_path('data',filename_calendar))
  smd_print("CREATED", smd_file_path('data',filename_calendar_import))
  smd_print("CREATED", smd_file_path('data',filename_calendar_import_college))
  
  # return filenames
  return(c(filename_calendar_import,filename_calendar,filename_calendar_import_college))
  
} # end function



