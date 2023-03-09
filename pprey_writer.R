# Alberto Rovellini
# 02/14/2023
# make new pprey matrix from prm file
# This code has the purpose of changing values of PPREY for a specific prey across all its predators.
# Example application 1: group XXX has little top-down control and we want to increase pressure on it across the board
# Example application 2: group XXX gets eaten too much and we want to reduce pressure on it
# Example application 3: group XXX has a very flat age structure, symptom that mortality on it is very low, and we do not want to use mL or mQ
# If your goal is to change a specific value of pPREYnXXXm, you should use Javier's tool in reactive atlantis

library(data.table)
library(tidyverse)

select <- dplyr::select

setwd('~/GOA/Parametrization/output_files/data')

# read in groups
atlantis_fg_tmp <- read.csv('GOA_Groups.csv')
atlantis_fg <- atlantis_fg_tmp %>% pull(Code)
atlantis_fg <- c(atlantis_fg, 'DCsed', 'DLsed', 'DRsed')

# pick run
this_run <- 1014

# read in prm file
prm_file <- paste0('out_', this_run, '/GOAbioparam_test.prm')
prm <- readLines(prm_file)

# identify rows where the pPREY matrix is
pprey_rows <- grep('pPREY',prm)
first_row <- pprey_rows[2] # discard the first one as it is a message in the GOA prm models
last_row <- pprey_rows[length(pprey_rows)]+2 # adding the vector of values and the empty row at the end

# read the pprey matrix
pprey_matrix <- prm[first_row:last_row]

# alternatively, read pprey matrix directly
# pprey_matrix <- readLines( paste0('out_', this_run, '/pprey_new/pprey_newtmp.prm'))

pprey_names <- pprey_matrix[grep('pPREY', pprey_matrix)] # get name rows
pprey_vals <- pprey_matrix[-grep('pPREY', pprey_matrix)] # get value rows

# doing some cleaning of the text
# clean empty rows
pprey_vals <- pprey_vals[which(nchar(pprey_vals)>0)]

# replace tabs with spaces
pprey_vals <- gsub('\t', ' ', pprey_vals)

# turn the pprey matrix to a data frame to manipulate
val_list <- list()

for(i in 1:length(pprey_vals)){
  
  # split each string assuming that values are separated by a space
  this_pprey_vec <- as.data.frame(t(as.numeric(unlist(strsplit(pprey_vals[i],' ')))))
  
  # drop NAs at the end of the string (cases where there was a string of blanks)
  this_pprey_vec <- this_pprey_vec %>% select_if(~ !any(is.na(.)))
  
  # write out
  val_list[[i]] <- this_pprey_vec
}

val_frame <- rbindlist(val_list) %>%
  set_names(atlantis_fg)

# add column with stage of the prey
val_frame1 <- val_frame2 <- val_frame %>% mutate(pprey = pprey_names,
                                  tmp = gsub('pPREY','', gsub('  81.*', '', pprey)),
                                  prey_stage = as.numeric(substr(tmp,1,1)),
                                  pred_stage = as.numeric(substr(tmp, nchar(tmp), nchar(tmp))),
                                  pred = gsub('2','', gsub('1','',tmp))) %>%
  select(pred, prey_stage, pred_stage, KWT:DRsed)

############################################################################################
# by column

# define which groups and stages should be changed
target_groups <- 'ZM'
target_prey_stage <- c(0, c(1,2)) # the stage of the prey for which we change values, 0 always present for the invertebrates that have no stage
target_pred_stage <- c(0, c(1,2)) # the stage of the predator, 0 always present for the invertebrates that have no stage
target_change <- 1/5

# change NA stage to 0 for inverts
val_frame2$prey_stage[is.na(val_frame2$prey_stage)] <- 0
val_frame2$pred_stage[is.na(val_frame2$pred_stage)] <- 0

# apply change
for(i in 1:length(target_groups)){
  
  target_group <- target_groups[i] # this should cycle through each target group once
  
  val_frame2 <- val_frame2 %>%
    rowwise() %>%
    mutate(!!target_group := if_else(prey_stage %in% target_prey_stage & pred_stage %in% target_pred_stage, 
                                     .data[[target_group]] * target_change,
                                     .data[[target_group]])) %>%
    ungroup()
  
}

val_frame2 <- val_frame2[,-c(1:3)]

# set to 0.9999 anything > 1, or else Atlantis will not run
val_frame2[val_frame2 >= 1] <- 0.999


# write out new pprey matrix
if(!dir.exists(paste0('out_', this_run, '/pprey_new'))){
  dir.create(paste0('out_', this_run, '/pprey_new'))
}

pprey_file <- paste0('out_', this_run, '/pprey_new/pprey_new',
                     '_predstage', paste0(target_pred_stage, collapse = '-'),
                     '_preystage', paste0(target_prey_stage, collapse = '-'),
                     '_multiplier',
                     gsub('\\.','',as.character(target_change)), '.prm')

# pprey_file <- paste0('out_', this_run, '/pprey_new/pprey_new',
#                      'tmp.prm')

file.create(pprey_file)

for(i in 1:length(pprey_names)){
  
  cat(pprey_names[i], file=pprey_file, append=TRUE,'\n')
  cat(unlist(val_frame2[i,]), file=pprey_file, append=TRUE, '\n')
  
}

#####################################################################
# Adding code that multiplies rows instead of columns
# So when you want to increase the PPREY pressure for a life stage on a life stage (or all)
# TODO: reorder this whole script so that it makes sense...

target_groups <- atlantis_fg_tmp %>% filter(GroupType %in% c('MAMMAL', 'FISH', 'BIRD', 'SHARK')) %>% pull(Code)
target_prey_stage <- c(0, c(1,2)) # the stage of the prey for which we change values, 0 always present for the invertebrates that have no stage
target_pred_stage <- c(0, c(1,2)) # the stage of the predator, 0 always present for the invertebrates that have no stage
target_change <- 2

# change NA stage to 0 for inverts
val_frame3 <- val_frame1

val_frame3$prey_stage[is.na(val_frame3$prey_stage)] <- 0
val_frame3$pred_stage[is.na(val_frame3$pred_stage)] <- 0

# apply change
for(i in 1:length(target_groups)){
  
  target_group <- target_groups[i] # this should cycle through each target group once
  
  val_frame3[val_frame3$pred == target_group & 
               val_frame3$prey_stage %in% target_prey_stage & 
               val_frame3$pred_stage %in% target_pred_stage, 7:ncol(val_frame3)] <- 
    val_frame3[val_frame3$pred == target_group & 
                 val_frame3$prey_stage %in% target_prey_stage & 
                 val_frame3$pred_stage %in% target_pred_stage, 7:ncol(val_frame3)] * target_change
  
}

val_frame3 <- val_frame3[,-c(1:3)]

# set to 0.9999 anything > 1, or else Atlantis will not run
val_frame3[val_frame3 >= 1] <- 0.999


# write out new pprey matrix
if(!dir.exists(paste0('out_', this_run, '/pprey_new'))){
  dir.create(paste0('out_', this_run, '/pprey_new'))
}

pprey_file <- paste0('out_', this_run, '/pprey_new/pprey_new',
                     '_predstage', paste0(target_pred_stage, collapse = '-'),
                     '_preystage', paste0(target_prey_stage, collapse = '-'),
                     '_multiplier',
                     gsub('\\.','',as.character(target_change)), '.prm')

# pprey_file <- paste0('out_', this_run, '/pprey_new/pprey_new',
#                      'tmp.prm')

file.create(pprey_file)

for(i in 1:length(pprey_names)){
  
  cat(pprey_names[i], file=pprey_file, append=TRUE,'\n')
  cat(unlist(val_frame3[i,]), file=pprey_file, append=TRUE, '\n')
  
}

