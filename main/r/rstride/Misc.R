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
#  Copyright 2019, Willem L, Kuylen E & Broeckhove J
#############################################################################
#
# HELP FUNCTIONS FOR rSTRIDE PRE- AND POST-PROCESSING                       
#
#############################################################################

#.rstride$set_wd()  #DEVELOPMENT: to set the work directory as the latest stride install dir 
#.rstride$load_pd() #DEVELOPMENT: to retrieve the latest project directory (project_dir)

# load required R package
require(doParallel,quietly = TRUE)

if(!(exists('.rstride'))){
  .rstride <- new.env()
}

# ###############################
# ## COMMAND LINE MESSAGES     ##
# ###############################

# terminate rStride
.rstride$cli_abort <- function()
{
  smd_print('!! TERMINATE rSTRIDE CONTROLLER !!',WARNING=T)
}

###############################
## PROJECT SUMMARY           ##
###############################

.rstride$load_project_summary <- function(project_dir, stringsAsFactors=FALSE){
  
  # check if project_dir exists
  if(.rstride$dir_not_present(project_dir)){
    stop('PROJECT DIR NOT PRESENT')
  }
  
  # get the filename in the project dir that contains "summary.csv" (full path)
  project_summary_filename <- file.path(project_dir,dir(project_dir,pattern = 'summary.csv'))
  
  # read the csv file
  project_summary          <- read.table(project_summary_filename,sep=',',header=T,stringsAsFactors = stringsAsFactors)
  
  # return the data.frame
  return(project_summary)
}

###############################
## OPEN PDF STREAM           ##
###############################

.rstride$create_pdf <- function(project_dir,file_name,width=7,height=7){
  
  # load project summary
  project_summary   <- .rstride$load_project_summary(project_dir)
  
  # get run_tag
  run_tag           <- unique(project_summary$run_tag)
  
  # get file name with path
  file_name_path    <- file.path(project_dir,paste0(run_tag,'_',file_name,'.pdf'))
  
  # open pdf stream
  pdf(file_name_path,width,height)
  
}

###############################
## CREATE EXPERIMENT TAG     ##
###############################

# create experiment tag
.rstride$create_exp_tag <- function(i_exp){
  
  # create experiment tag with leading 0's
  exp_tag <- paste0('exp',sprintf("%04s", i_exp))
  
  # solve issue with spaces instead of 0's on linux (leibniz)
  exp_tag <- gsub(' ','0',exp_tag)
  
  # return
  return(exp_tag)
}


###############################
## XML FUNCTIONS             ##
###############################

# list_config <- config_disease
# root_name <- 'disease'
# output_prefix <- 'sim_output'
# Save a list in XML format with given root node
.rstride$save_config_xml <- function(list_config,root_name,output_prefix){
  
  # setup XML doc (to add prefix)
  xml_doc = newXMLDoc()
  
  # setup XML root
  root <- newXMLNode(root_name, doc = xml_doc)
  
  # add list info
  smd_listToXML(root, list_config)
  
  # create filename
  filename <- paste0(output_prefix,'.xml')
  
  # xml prefix
  xml_prefix <- paste0(' This file is part of the Stride software [', format(Sys.time()), ']')
  
  # save as XML,
  # note: if we use an XMLdoc to include prefix, the line break dissapears...
  # fix: http://r.789695.n4.nabble.com/saveXML-prefix-argument-td4678407.html
  cat( saveXML( xml_doc, indent = TRUE, prefix = newXMLCommentNode(xml_prefix)),  file = filename) 
  
  # return the filename
  return(filename)
}

###############################
## MATRIX OPERATIONS         ##
###############################
# note: integers are converted to a string with the transpose operation of a mixed matrix...
# so, separate the comparison for numeric and non-numeric types
.rstride$get_equal_rows <- function(f_matrix,f_vector){
  
  # get numeric columns
  col_numeric      <- unlist(lapply(f_vector,is.numeric))
  
  # compare
  bool_numeric     <- as.logical(colSums(t(f_matrix[,names(f_vector)[col_numeric]]) == c(f_vector[col_numeric])) == sum(col_numeric))
  bool_not_numeric <- as.logical(colSums(t(f_matrix[,names(f_vector)[!col_numeric]]) == c(f_vector[!col_numeric])) == sum(!col_numeric))
  
  # return combined result
  return(bool_numeric & bool_not_numeric)
}

########################################
## AGGREGATE EXPERIMENT OUTPUT FILES  ##
########################################

.rstride$aggregate_compressed_output <- function(project_dir){
  
  # load project summary
  project_summary      <- .rstride$load_project_summary(project_dir)
  
  # get output files
  data_filenames <- unique(dir(file.path(project_summary$output_prefix),pattern='.RData',full.names = T))
  
  # get output types
  data_type_all <- names(get(load(data_filenames[1])))
  
  # loop over the output data types
  data_type <- data_type_all[3]
  for(data_type in data_type_all){
    
  # loop over all experiments, rbind
  i_exp <- 52
  data_all <- foreach(i_exp = 1:length(data_filenames),.combine='rbind') %dopar%
  {
        # get file name
        exp_file_name <- data_filenames[i_exp]
        
        # load output data
        data_exp_all    <- get(load(exp_file_name))
      
        # check if data type present, if not, next experiment
        if(!data_type %in% names(data_exp_all)){
          return(NULL)
        }
        
        # select output type
        data_exp        <- data_exp_all[[data_type]]
        
        # for prevalence data, check the number of days
        if(grepl('prevalence',data_type)){

          # create full-size data frame to include the maximum number of days
          data_tmp        <- data.frame(matrix(NA,ncol=max(project_summary$num_days)+2)) # +1 for day 0 and +1 for exp_id
          names(data_tmp) <-  c(paste0('day',0:max(project_summary$num_days)),
                                  'exp_id')

          # insert the experiment data
          data_tmp[names(data_exp)] <- data_exp

            # replace the experiment data by the newly constructed data.frame
            data_exp <- data_tmp
          }
          
          # add run index
          data_exp$exp_id <- project_summary$exp_id[i_exp]
        
          print(names(data_exp))
          # return
          data_exp
        } # end exp_id loop
            
    # make id's unique => by adding a exp_id tag with leading zero's
    names_id_columns  <- names(data_all)[grepl('id',names(data_all)) & names(data_all) != 'exp_id']
    num_exp_id_digits <- nchar(max(data_all$exp_id))+1
    
    if(length(names_id_columns)>0) {
      for(i_id_column in names_id_columns){
        row_is_id  <- !is.na(data_all[,i_id_column]) & data_all[,i_id_column] != 0
        data_all[row_is_id,i_id_column] <- as.numeric(sprintf(paste0('%d%0',num_exp_id_digits,'d'),
                                                              data_all[row_is_id,i_id_column],
                                                              data_all$exp_id[row_is_id]))
      }
    }
    
    # save
    run_tag <- unique(project_summary$run_tag)
    save(data_all,file=file.path(project_dir,paste0(run_tag,'_',data_type,'.RData')))
  } # end data-type loop
}

########################################
## AGGREGATE EXPERIMENT OUTPUT FILES  ##
########################################

.rstride$aggregate_exp_output <- function(project_dir){
  
  # load project summary
  project_summary      <- .rstride$load_project_summary(project_dir)
  
  # get output data types
  data_type_opt <- unique(dir(file.path(project_summary$output_prefix),pattern='.RData'))
  
  data_type <- data_type_opt[3]
  for(data_type in data_type_opt)
  {
    
    data_filenames <- dir(project_dir,pattern=data_type,recursive = T,full.names = T)
    
    # load all project experiments
    i_exp <- 1
    data_all <- foreach(i_exp = 1:nrow(project_summary),.combine='rbind') %dopar%
    {
      # get file name
      exp_file_name <- file.path(project_summary$output_prefix[i_exp],data_type)
      
      # check if output exists for the specified data_type
      if(file.exists(exp_file_name)){
        
        # load output data
        param_name  <- load(exp_file_name)
        
        # load data
        data_exp    <- get(param_name)
        
        # for prevalence data, check the number of days
        if(grepl('prevalence',exp_file_name)){
          
          # create full-size data frame to include the maximum number of days
          data_tmp        <- data.frame(matrix(NA,ncol=max(project_summary$num_days)+2)) # +1 for day 0 and +1 for exp_id
          names(data_tmp) <-  c(paste0('day',0:max(project_summary$num_days)),
                                'exp_id')
          
          # insert the experiment data
          data_tmp[names(data_exp)] <- data_exp
          
          # replace the experiment data by the newly constructed data.frame
          data_exp <- data_tmp
        }
        
        # add run index
        data_exp$exp_id <- project_summary$exp_id[i_exp]
        
        # return experiment data
        data_exp
      } # end 'if file exists'
    } # end exp_id loop
    
    # make id's unique => by adding a exp_id tag with leading zero's
    names_id_columns  <- names(data_all)[grepl('id',names(data_all)) & names(data_all) != 'exp_id']
    num_exp_id_digits <- nchar(max(data_all$exp_id))+1
    
    if(length(names_id_columns)>0) {
      for(i_id_column in names_id_columns){
        row_is_id  <- !is.na(data_all[,i_id_column]) & data_all[,i_id_column] != 0
        data_all[row_is_id,i_id_column] <- as.numeric(sprintf(paste0('%d%0',num_exp_id_digits,'d'),
                                                                    data_all[row_is_id,i_id_column],
                                                                    data_all$exp_id[row_is_id]))
      }
    }
      
    # save
    run_tag <- unique(project_summary$run_tag)
    save(data_all,file=file.path(project_dir,paste0(run_tag,'_',data_type)))
  } # end data-type loop
}

.rstride$load_aggregated_output <- function(project_dir,file_type,exp_id_opt = NA){
  
  # load project summary
  project_summary <- .rstride$load_project_summary(project_dir)
  
  # get ouput filenames
  dir_files       <- dir(project_dir,full.names = TRUE)
  output_filename <- dir_files[grepl(file_type,dir_files)]
  
  # if the file does not exists, return NA
  if(length(output_filename)==0){
    return(NA)
  }
  
  # load output
  param_name          <- load(output_filename)
  
  # rename parameter
  data_out <- get(param_name)
  
  # selection?
  if(!any(is.na(exp_id_opt))){
    data_out <- data_out[data_out$exp_id %in% exp_id_opt,]
  }
  
  # return
  return(data_out)
  
}


###############################
## DEFENSIVE PROGRAMMING     ##
###############################

.rstride$no_return_value <- function(){
  return(invisible())
}

.rstride$dir_not_present <- function(path_dir){
  
  # if directory does not exists, return TRUE (+warning)
  if(!file.exists(paste(path_dir))){
    smd_print('[',paste(match.call(),collapse=' '),'] DIRECTORY NOT PRESENT:',path_dir,WARNING=T)
    return(TRUE)
  } 
  
  # else, return FALSE
  return(FALSE)
}

# check file presence
.rstride$data_files_exist <- function(design_of_experiment = exp_design){
  
  # get the unique file names
  file_names <- unique(c(design_of_experiment$age_contact_matrix_file,
                         design_of_experiment$disease_config_file,
                         design_of_experiment$holidays_file,
                         design_of_experiment$population_file))
  
  # add the path to the data folder
  data_dir <- './data'
  file_names <- file.path(data_dir,file_names)
  
  # check the existance of the files
  file_not_exist_bool   <- !file.exists(file_names)
  
  # if any file missing => return FALSE
  if(any(file_not_exist_bool)){
    smd_print('DATA FILE(S) MISSING:', paste(file_names[file_not_exist_bool],collapse = ' '),WARNING=T)
    return(FALSE)
  }  
  
  # else => return TRUE
  return(TRUE)
}

# log level
# check file presence
.rstride$log_levels_exist <- function(design_of_experiment = exp_design){
  
  valid_levels <- design_of_experiment$contact_log_level %in% c('None','Transmissions','All')
  
  if(any(!valid_levels)){
    smd_print('INVALID LOG LEVEL(S):', paste(design_of_experiment$contact_log_level[!valid_levels],collapse = ' '),WARNING=T)
    return(FALSE)
  }  
  
  # else => return TRUE
  return(TRUE)
  
}

# R0
.rstride$valid_r0_values <- function(design_of_experiment = exp_design){
  
  if(any(!is.null(design_of_experiment$r0)))
  {
    
    r0_max <- max(design_of_experiment$r0)
    
    for(disease_config_file in unique(design_of_experiment$disease_config_file)){
      
      # load disease config file
      config_disease    <- xmlToList(file.path('data',disease_config_file))
      
      # get R0 limit    
      fit_r0_limit <- as.numeric(config_disease$label$fit_r0_limit)
      
      # check
      if(r0_max > fit_r0_limit){
        smd_print('INVALID R0 CONFIG VALUE(S):', paste(design_of_experiment$r0,collapse = ' '),paste0('(R0 LIMIT = ',fit_r0_limit,')') ,WARNING=T)
        return(FALSE)
      } 
    } # end for-loop
  }
  
  # else => return TRUE
  return(TRUE)
  
}

# immunity
.rstride$valid_immunity_profiles <- function(design_of_experiment = exp_design){
  
  immunity_profiles <- unique(c(design_of_experiment$immunity_profile,design_of_experiment$vaccine_profile))
  
  # get immunity profile names
  disease_immunity_profiles <- c('None','Random','AgeDependent','Cocoon')
  
  # check if given profile names are valid
  if(!all(immunity_profiles %in% disease_immunity_profiles)){
    smd_print('INVALID IMMUNITY PROFILE(S):', paste(immunity_profiles,collapse = ' '),WARNING=T)
    return(FALSE)
  } # end if-compare
  
  # else => return TRUE
  return(TRUE)
  
}

# immunity
.rstride$valid_seed_infected <- function(design_of_experiment = exp_design){
  
  # select unique combinations of population file and seeding rate
  unique_exp_design <- unique(design_of_experiment[,c('population_file','seeding_rate')])
  
  # add the path to the data folder
  data_dir <- './data'
  unique_exp_design$population_file_full <- file.path(data_dir,unique_exp_design$population_file)
  
  # count lines
  unique_exp_design$population_size <- NA
  for(i_file in 1:nrow(unique_exp_design)){
    file_connnection                         <- file(unique_exp_design$population_file_full[i_file]) 
    unique_exp_design$population_size[i_file] <- length(readLines(file_connnection)) - 1              # -1 for header  
    close(file_connnection)
  }
  
  # calculate the number of infected seeds
  unique_exp_design$num_seed_infected <- floor(unique_exp_design$seeding_rate * unique_exp_design$population_size)
  
  # check... and print warning if needed
  if(any(unique_exp_design$num_seed_infected<=0)){
    smd_print('NUMBER OF INFECTED SEEDS == 0 WITH:', paste(unique_exp_design[unique_exp_design$num_seed_infected<=0,1:2], collapse = ' & seeding rate '),WARNING=T)
    return(FALSE)
  }
  
  # else => return TRUE
  return(TRUE)
  
}



###############################
## DEVELOPMENT FUNCTIONS     ##
###############################

# load last project_dir
.rstride$load_pd <- function(){
  
  # set most recent build as work directory
  .rstride$set_wd()
  
  # default output dir
  output_dir              <- 'sim_output'
  
  # load directory content (non recursive)
  sim_dirs <- dir(output_dir) 
  
  # create project_dir (global)
  project_dir <<- file.path(output_dir,sim_dirs[length(sim_dirs)])
  
  # terminal message
  smd_print('SET PROJECT DIR TO ', project_dir)
  
  # load rStride source code
  source('bin/rstride/rStride.R')
  smd_print('rSTRIDE LOADED')
  
}

# set most recent stride install directory as work directory 
#.rstride$set_wd()
.rstride$set_wd <- function(){
  
  # default install directory
  install_dir              <- system('echo $HOME/opt',intern=T)
  
  # load directory content (non recursive)
  stride_dir_tag  <- 'stride-'
  stride_dirs     <- dir(install_dir,pattern = stride_dir_tag)
  stride_dirs_num <- as.numeric(sub(stride_dir_tag,'',stride_dirs))
  
  # select last directory
  last_stride_dir <- stride_dirs[stride_dirs_num == max(stride_dirs_num)]
  
  # set work directory
  setwd(file.path(install_dir,last_stride_dir))
  
  # terminal message
  smd_print('NEW WORK DIRECTORY ',file.path(install_dir,last_stride_dir))
  
}
