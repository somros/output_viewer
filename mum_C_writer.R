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
this_run <- 1065

# read in prm file
prm_file <- paste0('out_', this_run, '/GOAbioparam_test.prm')
prm <- readLines(prm_file)

# create folder
if(!dir.exists(paste0('out_', this_run, '/mum_C_new'))){
  dir.create(paste0('out_', this_run, '/mum_C_new'))
}

# All age classes ---------------------------------------------------------

# # set multiplier
# multiplier <- 0.7
# 
# # pick target groups 
# target_groups <- vert_groups
# 
# # target param
# target_param <- c('mum')#,'C')
# 
# mum_file <- paste0('out_', this_run, '/mum_C_new/mum_', multiplier, '.prm')
# C_file <- paste0('out_', this_run, '/mum_C_new/C_', multiplier, '.prm')
# 
# if(length(target_param) == 2){
#   file.create(c(mum_file, C_file))
# } else if (target_param == 'mum') {
#   file.create(mum_file)
# } else {
#   file.create(C_file)
# }
# 
# for(i in 1:length(target_groups)){
#   
#   for(j in 1:length(target_param)){
#     
#     if(target_param[j] == 'mum'){
#       par_file <- mum_file
#     } else {
#       par_file <- C_file
#     }
#     
#     this_group <- target_groups[i]
#     this_param <- target_param[j]
#     
#     par_row_idx <- grep(paste0(this_param, '_', this_group), prm)
#     
#     par_row <- prm[par_row_idx]
#     par_vec <- as.numeric(unlist(strsplit(prm[par_row_idx+2], ' ')))
#     par_vec <- par_vec[!is.na(par_vec)]
#     
#     new_par_vec <- par_vec * multiplier
#     
#     cat(par_row, file=par_file, append=TRUE,'\n\n')
#     cat(new_par_vec, file=par_file, append=TRUE, '\n\n\n')
#     
#   }
#   
# }

# By age class ------------------------------------------------------------
# scale by age class based on input CSV table that contains the values of the scalars for
# each group and the age classes to scale
# for now do on C only

target_groups <- vert_groups

# read scalar table from file
scalarvec <- read.csv(paste0(getwd(), '/', paste0('out_', this_run, '/mum_C_new/'), 'scalarvec.csv'))

# create a blank file to write the modified C vectors to
C_file <- paste0('out_', this_run, '/mum_C_new/C_scaled_', substr(Sys.time(), 1, 10), '.prm')

if(file.exists(C_file)) stop('This file exists')

# loop over species
for(i in 1:length(target_groups)){
  
# first, remove agevec1 and agevec2 at the beginning of each iteration
  rm(agevec1)
  rm(agevec2)
  
  this_group <- target_groups[i]
  
  print(paste('Doing', this_group, '(', i, ')'))

  par_row_idx <- grep(paste0('C_', this_group), prm)
  
  par_row <- prm[par_row_idx]
  par_vec <- as.numeric(unlist(strsplit(prm[par_row_idx+2], ' ')))
  par_vec <- par_vec[!is.na(par_vec)]
  
  # if this group is not getting modified, write to file and end this iteration of the loop...
  if(this_group %in% setdiff(target_groups, scalarvec$Code)){
    cat(par_row, file=C_file, append=TRUE,'\n\n')
    cat(par_vec, file=C_file, append=TRUE, '\n\n\n')
    next
  }
  
  # ... else, prepare scalars
  scalar1 <- scalarvec %>% 
    filter(Code == this_group) %>% 
    mutate(Scalar1 = ifelse(Mult > 0, Scalar1, 1/Scalar1)) %>%
    pull(Scalar1)
  
  scalar2 <- scalarvec %>% 
    filter(Code == this_group) %>% 
    mutate(Scalar2 = ifelse(Mult > 0, Scalar2, 1/Scalar2)) %>%
    pull(Scalar2)
  
  # if at least one agevec exists, get the extremes and construct it
  if((scalarvec %>% 
            filter(Code == this_group) %>%
            pull(Agevec1)) != '') {
    # get age range for scalar 1
    agerange1 <- as.numeric(
      unlist(
        strsplit(
          gsub(
            '\\)', '', gsub(
              '\\(', '', scalarvec %>% 
                filter(Code == this_group) %>%
                pull(Agevec1)
            )
          ), '-')
      )
    )
    
    agevec1 <- seq(agerange1[1], agerange1[2])
    # if it exists, do the same for agevec2
    
    if((scalarvec %>% 
        filter(Code == this_group) %>%
        pull(Agevec2)) != '') {
      agerange2 <- as.numeric(
        unlist(
          strsplit(
            gsub(
              '\\)', '', gsub(
                '\\(', '', scalarvec %>% 
                  filter(Code == this_group) %>%
                  pull(Agevec2)
              )
            ), '-')
        )
      )
      
      agevec2 <- seq(agerange2[1], agerange2[2])
    }
  }
  
  new_par_vec <- par_vec
  # apply scalars if they exist
  if(exists('agevec1')){
    new_par_vec[agevec1] <- new_par_vec[agevec1] * scalar1
    
    if(exists('agevec2')) {
      new_par_vec[agevec2] <- new_par_vec[agevec2] * scalar2
    }
  } else {
    new_par_vec <- new_par_vec * scalar1
  }
  
  # write to file
  cat(par_row, file=C_file, append=TRUE,'\n\n')
  cat(new_par_vec, file=C_file, append=TRUE, '\n\n\n')
  
}
