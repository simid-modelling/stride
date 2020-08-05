############################################################################ #
#  This file is part of the Stride software. 
#
#  Copyright 2020, Willem L
############################################################################ #
#
# TO OBTAIN A CALENDAR FILE
#
# CONTAINS:
# 1. Public holidays
# 2. Contact reductions
#       * pre-, primairy and secondary school
#       * workplace
#       * community
#       * household clusters
# 3. Imported cases
# 4. Contact tracing
# 5. Universal testing
#
############################################################################ #

# clear workspace
rm(list=ls())

# load packages
suppressPackageStartupMessages(library(simid.rtools)) # to save a list as XML

# load rStride functions
source('./bin/rstride/rStride.R')

########################################### #
## INITIATE DATA                       ####
########################################### #

# set default value in C++ CALENDAR lists ==>> 0


########################################### #
## 1.a Public holidays                 ####
########################################### #

data.table(category = "general",
           date     = as.Date(c("2020-01-01",'2020-04-13','2020-05-01','2020-05-21','2020-06-01',
                      '2020-07-21','2020-08-15','2020-11-01','2020-11-11','2020-12-25')),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F) -> d_calendar_holiday

summary(d_calendar_holiday)

########################################### #
## 1.b School holidays                 ####
########################################### #

# set default "as.Date origin"
date_origin <- as.Date('2020-01-01') - as.numeric(as.Date('2020-01-01'))

data.table(category = "school_holiday",
           date     = as.Date(c(as.Date('2020-01-01'):as.Date('2020-01-05'),
                                  as.Date('2020-02-24'):as.Date('2020-02-29'),
                                  as.Date('2020-04-06'):as.Date('2020-04-19'),
                                  as.Date('2020-07-01'):as.Date('2020-08-31'),
                                  as.Date('2020-11-02'):as.Date('2020-11-08'),
                                  as.Date('2020-12-21'):as.Date('2020-12-31')),
                                origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> d_school_holidays

data.table(category = "college",
           date     = as.Date(c(as.Date('2020-01-01'):as.Date('2020-01-05'),
                                as.Date('2020-02-24'):as.Date('2020-02-29'),
                                as.Date('2020-04-06'):as.Date('2020-04-19'),
                                as.Date('2020-07-01'):as.Date('2020-09-20'), # untill September, 20
                                #as.Date('2020-11-02'):as.Date('2020-11-08'), # no fall break
                                as.Date('2020-12-21'):as.Date('2020-12-31')),
                              origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> d_college_holidays

d_calendar_holiday <- rbind(d_calendar_holiday,d_school_holidays[,category := 'preschool'])
d_calendar_holiday <- rbind(d_calendar_holiday,d_school_holidays[,category := 'primary_school'])
d_calendar_holiday <- rbind(d_calendar_holiday,d_school_holidays[,category := 'secondary_school'])
d_calendar_holiday <- rbind(d_calendar_holiday,d_college_holidays)

########################################### #
##  2a. School closures                 ####
########################################### #
#       * (pre-, primairy and secondary school)

# set default school closure
data.table(category = "school_closure",
           date     = seq(as.Date('2020-03-14'),as.Date('2020-06-30'),1),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> d_school_closure

# set eligible dates for school reopening
d_school_reopening <- seq(as.Date('2020-05-18'),as.Date('2020-06-30'),1)
d_school_reopening_wday <- as.POSIXlt(d_school_reopening)$wday

# preschool (reopens 4d/week)
d_preschool <- d_school_reopening[d_school_reopening_wday %in% 1:4]
dcal_preschool_closure <- d_school_closure[!date %in% d_preschool,]
dcal_preschool_closure[,category := 'preschool']

# primary school (reopens 2d/week)
d_primary <- d_school_reopening[d_school_reopening_wday %in% 4:5]
d_primary[1:2] <- d_primary[1:2] - 2 # fix for holidays Thu-Fri in May
dcal_primary_closure <- d_school_closure[!date %in% d_primary,]
dcal_primary_closure[,category := 'primary_school']

#secondary school (reopens 1d week)
d_secondary <- d_school_reopening[d_school_reopening_wday %in% 3]
d_secondary <- c(d_secondary,d_school_reopening[length(d_school_reopening)]) # add one day in last week
dcal_secondary_closure <- d_school_closure[!date %in% d_secondary,]
dcal_secondary_closure[,category := 'secondary_school']

# college (stay closed)
dcal_college_closure <- d_school_closure
dcal_college_closure[,category := 'college']

########################################### #
##  2b. Contact reductions               ####
########################################### #
#       * workplace
#       * community
#       * household clusters

# workplace distancing
data.table(category = "workplace_distancing",
           date     = as.Date(c(as.Date('2020-03-14'):as.Date('2020-05-03')),
                              origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> dcal_workplace_distancing


# workplace distancing
data.table(category = "community_distancing",
           date     = as.Date(c(as.Date('2020-03-14'):as.Date('2020-05-03')),
                              origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> dcal_community_distancing

# household clustering
data.table(category = "household_clustering",
           date     = as.Date(c(as.Date('2020-05-10'):as.Date('2020-08-31')),
                              origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> dcal_household_clustering

########################################### #
## 3. Imported cases                 ####
########################################### #

data.table(category = "import_cases",
           date     = as.Date(c(as.Date('2020-07-01'):as.Date('2020-08-31')),
                              origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> dcal_imported_cases

########################################### #
##  4. Contact tracing                 ####
########################################### #

data.table(category = "contact_tracing",
           date     = as.Date(c(as.Date('2020-05-11'):as.Date('2020-08-31')),
                              origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> dcal_contact_tracing

########################################### #
## 5. Universal testing                 ####
########################################### #

data.table(category = "universal_testing",
           date     = as.Date(c(as.Date('2020-05-11'):as.Date('2020-08-31')),
                              origin = date_origin),
           value    = 1,
           type = 'boolean',
           stringsAsFactors = F
) -> dcal_universal_testing


########################################### #
## MERGE HOLIDAYS & 'OTHER'             ####
########################################### #

# get 'dcal_*' variables
opt_other <- ls(pattern='dcal_')

# combine all 'dcal_*' variable
d_calendar_all <- foreach(i_other  = opt_other,
                    .init    = d_calendar_holiday,
                    .combine = 'rbind') %do% {
                      get(i_other)
                    } 

########################################### #
## EXPLORE DATA                         ####
########################################### #

# open pdf stream
#pdf(file='./sim_output/calendar_profile.pdf',6,6)

category_opt <- levels(as.factor(d_calendar_all$category))

category_opt <- unique(d_calendar_all$category)
category_opt[!duplicated(category_opt)]
par(mfrow=c(4,1))

x_lim <- range(d_calendar_all$date)


i_cat <- category_opt[1]
for(i_cat in category_opt){
        plot(x   = d_calendar_all[category == i_cat,date],
             y   = d_calendar_all[category == i_cat,value],
             xlim = x_lim,
             col  = 1,
             pch  = 15,
             main = i_cat,
             xlab = '',
             ylab = unique(d_calendar_all[,type]),
             xaxt = 'n'
        )
        add_x_axis(x_lim,num_ticks = 12 )      
}


# close pdf stream
#dev.off()

########################################### #
## SAVE AS XML AND CSV	 	         ####
########################################### #

# # format date
# d_calendar_all[,date:=format(date,'%Y-%m-%d')]
# format(d_calendar_all$date,'%Y-%m-%d')

# save as csv (none ==>> dummy)
write.table(d_calendar_holiday[category == 'na',],
            file = 'sim_output/holidays_none.csv',sep=',',row.names=F,quote=F)

# save as csv (default holidays)
write.table(d_calendar_holiday,
            file = 'sim_output/holidays_flanders_2020.csv',sep=',',row.names=F,quote=F)

# save as csv (selection)
write.table(d_calendar_all[!category %in% c('universal_testing','import_cases'),],
            file = 'sim_output/calendar_belgium_2020_covid19_exit_school_adjusted.csv',sep=',',row.names=F,quote=F)

# save as csv (selection)
write.table(d_calendar_all[!category %in% c('import_cases'),],
            file = 'sim_output/calendar_belgium_2020_covid19_exit_school_adjusted_inversal.csv',sep=',',row.names=F,quote=F)


# save as csv (all calendar info)
write.table(d_calendar_all,
            file = 'sim_output/calendar_belgium_2020_covid19_exit_school_adjusted_universal_import.csv',sep=',',row.names=F,quote=F)

unique(d_calendar_all$category)

