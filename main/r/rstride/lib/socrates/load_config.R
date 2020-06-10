#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => DEFINE USER INTERFACE AND MODELLING OPTIONS 
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

#__________________________#
##  R-PACKAGES          ####
#__________________________#
# socialmixr    to process social contact data
# npsp          to use 'simage' in plot_cnt_matrix
# countrycode   to convert country names into iso3 codes
# data.table    to adapt socialmixr::contact_matrix

# Explicit loading of the packages (fix for www.shinyapps.io)
library('socialmixr')
library('countrycode')
library('data.table')
library(shiny)

# temporary to use the get_survey script outside the SocialMixr package
library(httr)
library(jsonlite)
library(XML)
library(curl)

#__________________________#
##  WEIGHTS             ####
#__________________________#

## MAXIMUM WEIGHT
max_part_weight <- 3


#__________________________#
##  FORMATTING          ####
#__________________________#

# number of digits to round
format_num_digits <- 2

