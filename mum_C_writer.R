# ALberto Rovellini
# 3/8/2023
# Code to scale values of C and/or mum for vertebrates from a PRM file

library(data.table)
library(tidyverse)

select <- dplyr::select

setwd('~/GOA/Parametrization/output_files/data')

# read in groups
atlantis_fg_tmp <- read.csv('GOA_Groups.csv')
atlantis_fg <- atlantis_fg_tmp %>% pull(Code)
vert_groups <- atlantis_fg_tmp %>% filter(GroupType %in% c('FISH', 'MAMMAL', 'SHARK', 'BIRD')) %>% pull(Code) %>% sort()

# pick run
this_run <- 1014

# set multiplier
multiplier <- 0.7

# pick target groups 
target_groups <- vert_groups

# target param
target_param <- c('mum')#,'C')

# read in prm file
prm_file <- paste0('out_', this_run, '/GOAbioparam_test.prm')
prm <- readLines(prm_file)

# write out new file
if(!dir.exists(paste0('out_', this_run, '/mum_C_new'))){
  dir.create(paste0('out_', this_run, '/mum_C_new'))
}

mum_file <- paste0('out_', this_run, '/mum_C_new/mum_', multiplier, '.prm')
C_file <- paste0('out_', this_run, '/mum_C_new/C_', multiplier, '.prm')

if(length(target_param) == 2){
  file.create(c(mum_file, C_file))
} else if (target_param == 'mum') {
  file.create(mum_file)
} else {
  file.create(C_file)
}

for(i in 1:length(target_groups)){
  
  for(j in 1:length(target_param)){
    
    if(target_param[j] == 'mum'){
      par_file <- mum_file
    } else {
      par_file <- C_file
    }
    
    this_group <- target_groups[i]
    this_param <- target_param[j]
    
    par_row_idx <- grep(paste0(this_param, '_', this_group), prm)
    
    par_row <- prm[par_row_idx]
    par_vec <- as.numeric(unlist(strsplit(prm[par_row_idx+2], ' ')))
    par_vec <- par_vec[!is.na(par_vec)]
    
    new_par_vec <- par_vec * multiplier
    
    cat(par_row, file=par_file, append=TRUE,'\n\n')
    cat(new_par_vec, file=par_file, append=TRUE, '\n\n\n')
    
  }
  
}
