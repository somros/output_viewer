# script to zip output folders to be uploaded on NOAA drive for backup
this.path <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/'

setwd(this.path)

all.folders <- list.dirs('.', recursive = F)
out.folders <- all.folders[grepl('out', all.folders)]
out.folders <- gsub('./', '', out.folders)

for(i in 1:length(out.folders)){
  zip(zipfile = out.folders[i], files = out.folders[i])
}

