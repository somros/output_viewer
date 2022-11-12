# read export.ts file
library(tidyverse)
library(rbgm)
library(data.table)
library(lubridate)
library(sf)
library(RColorBrewer)

select <- dplyr::select

this_path <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/'
this_dir <- 'out_392'

setwd(paste(this_path, this_dir, sep='/'))

exp_file <- 'export.ts'
exp_lines <- readLines(exp_file) # scan file

n_cols <- as.numeric(gsub("## COLUMNS ", "", exp_lines[grepl("COLUMNS", exp_lines)]))

col_names <- vector(mode = 'list', length = n_cols)

for(i in 1:n_cols){
  
  # find row
  this_name <- paste0("## COLUMN", i, '.name')
  # this_long_name <- paste0("## COLUMN", i, '.long_name')
  # this_unit <- paste0("## COLUMN", i, '.units')
  # this_missing_value <- paste0("## COLUMN", i, '.missing_value')
  
  # get index
  name_idx <- exp_lines[grepl(this_name, exp_lines)]
  # long_name_idx <- exp_lines[grepl(this_long_name, exp_lines)]
  # unit_idx <- exp_lines[grepl(this_unit, exp_lines)]
  # missing_value_idx <- exp_lines[grepl(this_missing_value, exp_lines)]
  
  # get column names
  name_val <- gsub(paste0(this_name, " "), "", name_idx)
  # long_name_val <- gsub(paste0(this_long_name, " "), "", long_name_idx)
  # unit_val <- gsub(paste0(this_unit, " "), "", unit_idx)
  # missing_value_val <- gsub(paste0(this_missing_value, " "), "", missing_value_idx)
  
  # for now only spit out name
  col_names[[i]] <- name_val
  
}

col_names <- unlist(col_names)

