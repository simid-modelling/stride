#############################################################################
#  This file is part of the Stride software. 
#
#  Copyright 2020, Willem L
#############################################################################
#
# TO CREATE AN AGE-SPECIFIC PROFILE FOR SYMPTOMATIC CASES  
#
#############################################################################

# clear workspace
rm(list=ls())

library(XML)
par(mfrow=c(1,2),mar=c(5,5,5,5))

############################################
## LOAD AND RESHAPE DATA                  ##
############################################

# Set the average symptomatic proportion per age (based on Wu et al, 
# Nature Medicine, Letter, figure 2b)
age_profile         <- c(rep(0.16,10),  # 0-9y
                         rep(0.16,10),  # 10-19y
                         rep(0.4,10),  # 20-29y
                         rep(1.0,10),  # 30-39y
                         rep(1.3,10),  # 40-49y
                         rep(2.0,10),  # 50-59y
                         rep(2.9,10),  # 60-69y
                         rep(3.0,10),  # 70-79y
                         rep(2.5,29)   # +79y
                         )

# plot
plot(0:(length(age_profile)-1),age_profile,
     pch=15,
     ylab='Relative susceptibility\nto symptomatic infection',
     xlab='Age (years)',
     main='Data from Wu et al,\nNature Medicine, 2020 (figure 2b)',
     ylim=c(0,3.1)
     )
grid(col=8)
points(0:19,age_profile[1:20],col=2,pch=15)
legend('topleft',
       c('Wu et al (2020)',
         'Assumption'),
       pch=15,
       col =1:2,
       title='Source')

# set overall fraction symptomatic of 50%
reference_fraction <- 0.5

# rescale age-specific fractions
symptomatic_profile <- age_profile / mean(age_profile) * reference_fraction
mean(symptomatic_profile)

# extend to 110 years of age by repeating the last one (from 0 to 110 year = 111 categories)
max_age      <- 110
num_age      <- max_age+1
num_age_data <- length(symptomatic_profile)
symptomatic_profile <- c(symptomatic_profile,rep(symptomatic_profile[num_age_data],num_age-num_age_data))

# explore
plot(symptomatic_profile,ylim=0:1,type='p',lwd=3,pch=15,
     ylab='Proportion symptomatic',
     xlab='Age (years)',
     main='Age profile for Stride',
     xlim=c(0,115))
abline(h=mean(symptomatic_profile),col=4,lwd=3)
#abline(h=0:1)
grid(col=8)
legend('topleft',
       'Mean',
       col=4,
       lwd=2)

text(x = which(diff(c(1,symptomatic_profile)) != 0),
     y = round(unique(symptomatic_profile),digits=2),
     round(unique(symptomatic_profile),digits=2),
     pos=3,
     cex=0.8)

text(x = 0,
     y = round(mean(symptomatic_profile),digits=2),
     round(mean(symptomatic_profile),digits=2),
     pos=3,
     col=4,
     cex=0.8)


#```{r chunk_name, include=FALSE}
############################################
## SAVE AS XML  	 	                      ##
############################################

# add age group as column names
names(symptomatic_profile) <- paste0('age',0:max_age)

# add info on data source and manipulation
out_data <- unlist(list(data_source = 'Wu et al, Nature Medicine + baseline assumption of overall fraction of 50%',
                             data_manipulation = "value by age-group",
                             round(symptomatic_profile,digits=4)))

# save as xml
.rstride$save_config_xml(out_data,'prob_symptomatic','symptomatic_covid19')
#```


xx <-data.frame(rbind(c(57, 2020-2004, 2020-2020),
                c(658, 2020-1960, 2020-2004),
                c(770, 2020-1940, 2020-1960),
                c(416, 2020-1900, 2020-1940)))
names(xx) <- c('count','age_max','age_min')

xx$count / sum(xx$count)


