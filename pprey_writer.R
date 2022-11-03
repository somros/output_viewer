# make new pprey matrix after manipulation in Excel
library(data.table)
library(tidyverse)

setwd('~/GOA/Parametrization/output_files/data/pprey_mats/')

template <- readLines('pprey_template.txt')
template <- template[grep('pPREY',template)]

pprey <- read.csv('../out_213/pprey213.csv')

header.file <- 'pprey_mat_213.txt'

for(i in 1:nrow(pprey)){
  cat(template[i], file = header.file, append = T)
  cat('\n', file = header.file, append = T)
  cat(unlist(pprey[i,]), file = header.file, append = T)
  cat('\n \n', file = header.file, append = T)
}


# Reduce pressure on a group ----------------------------------------------

setwd('~/GOA/Parametrization/output_files/data')

target_group <- 'DOL'
target_change <- 300

# read in groups
atlantis_fg <- read.csv('GOA_Groups.csv')
atlantis_fg <- atlantis_fg %>% pull(Code)
atlantis_fg <- c(atlantis_fg, 'DCsed', 'DLsed', 'DRsed')

# Can we write code to apply a modifier to the predation pressure on a group?

prm_file <- 'out_354/GOAbioparam_test.prm'

prm <- readLines(prm_file)

# identify rows where the pPREY matrix is
pprey_rows <- grep('pPREY',prm)
first_row <- pprey_rows[2] # discard the first one as it is a message in the GOA prm models
last_row <- pprey_rows[length(pprey_rows)]+2 # adding the vector of values and the empty row at the end 

# read the pprey matrix
pprey_matrix <- prm[first_row:last_row]

pprey_names <- pprey_matrix[grep('pPREY', pprey_matrix)]
pprey_vals <- pprey_matrix[-grep('pPREY', pprey_matrix)]

# make a data frame for the values
val_list <- list()

for(i in 1:length(pprey_vals)){
  val_list[[i]] <- as.data.frame(t(as.numeric(unlist(strsplit(pprey_vals[i],' ')))))
}

val_frame <- rbindlist(val_list) %>%
  set_names(atlantis_fg)

# apply change

val_frame <- val_frame %>%
  mutate(target_group = target_group * target_change)





