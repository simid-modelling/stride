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
# MODEL CONTACT TRACING EXPLORATION
#
############################################################################# #

#' @param project_dir   name of the project folder
#' @param num_selection the number of experiments with minimal LS score to select and present
inspect_tracing_data <- function(project_dir)
{
  # command line message
  smd_print('INSPECT CONTACT TRACING DATA...')
  
  # load project summary
  project_summary    <- .rstride$load_project_summary(project_dir)
  
  # retrieve all variable model parameters
  input_opt_design   <- .rstride$get_variable_model_param(project_summary)
  
  # get all tracing output
  data_tracing_all <- .rstride$load_aggregated_output(project_dir,'data_tracing')
  
  if(length(data_tracing_all) == 1 && is.na(data_tracing_all)){
    smd_print('NO CONTACT TRACE DATA AVAILABLE.')
    return(NA)
  }
  
 
  # add config_id 
  # get variable names of input_opt_design (fix if only one column)
  if(ncol(input_opt_design) == 1) {
    project_summary$config_id  <- project_summary[,colnames(input_opt_design)]
    input_opt_design           <- data.frame(input_opt_design,config_id = c(input_opt_design))
  } else{
    project_summary$config_id  <- apply(project_summary[,names(input_opt_design)],1,paste, collapse='_')
    input_opt_design$config_id <- apply(input_opt_design,1,paste, collapse='_')
  }
  
  # add config_id to incidence data
  data_tracing_all         <- merge(data_tracing_all,project_summary[,c('exp_id','config_id','start_date')] )

  ## ENSEMBLE  ####
  .rstride$create_pdf(project_dir,'contact_tracing',width = 7, height = 7)

  opt_config <- unique(project_summary$config_id)
  i_config <- 1
  for(i_config in 1:length(opt_config)){
    
    data_tracing_sel <- data_tracing_all[data_tracing_all$config_id == opt_config[i_config],]
    
    # add date
    sim_start_date <- as.Date(unique(data_tracing_sel$start_date))
    data_tracing_sel$sim_day_date <- sim_start_date + data_tracing_sel$sim_day
    
    # add value to aggregate
    data_tracing_sel$num_tests <- 1
    head(data_tracing_sel)
    
    # index cases
    data_tracing_index       <- data_tracing_sel[data_tracing_sel$pool_type == '-1',]
    tracing_num_day_index    <- aggregate(num_tests ~ sim_day_date + exp_id + config_id, data = data_tracing_index, sum)
    tracing_num_day_contacts <- aggregate(num_contacts_tested ~ sim_day_date + exp_id + config_id, data = data_tracing_index, sum)
    
    # contacts in quarentine
    data_tracing_contacts <- data_tracing_sel[data_tracing_sel$pool_type != '-1',]
    tracing_num_day_identified <- aggregate(num_tests ~ sim_day_date + exp_id + config_id, data = data_tracing_contacts, sum)
    
    y_lim <- range(0,tracing_num_day_index$num_tests)
    boxplot(num_tests ~ sim_day_date, data=tracing_num_day_index,
            ylab='index cases',main=opt_config[i_config],
            ylim = y_lim,las=2,xlab='',cex=0.8)
    boxplot(num_contacts_tested ~ sim_day_date, data=tracing_num_day_contacts,
            las=2,ylab='total number of contacts tested',main=opt_config[i_config])
    boxplot(num_tests ~ sim_day_date, data=tracing_num_day_identified,
            las=2,ylab='infected case identified',main=opt_config[i_config])
    
    }
  
  # close pdf stream
  dev.off()
  
  # command line message
  smd_print('INSPECTION OF CONTACT TRACING DATA COMPLETE')
}

