# copy forcing files
# filenames to contain indeces

# 30 x 2017
# 5 x 2014
# 5 x 2017

files <- list.files('../data/out_753/newfiles/forcings/', full.names = F)
files_long <- list.files('../data/out_753/newfiles/forcings/', full.names = T)

files2014 <- files[grepl('2014', files)]
files2017 <- files[grepl('2017', files)]
files2014_long <- files_long[grepl('2014', files_long)]
files2017_long <- files_long[grepl('2017', files_long)]

for(i in 1:length(files2017)){
  for(j in 1:30){
    if(j<10){jj <- paste0(0,j)}else{jj <- j}
    file.copy(files2017_long[i], paste0('../data/out_753/newfiles/dummy_forcings/', 'f', jj, '_', files2017[i]))
  }
}

for(i in 1:length(files2014)){
  for(j in 31:35){
    file.copy(files2014_long[i], paste0('../data/out_753/newfiles/dummy_forcings/', 'f', j, '_', files2014[i]))
  }
}

for(i in 1:length(files2017)){
  for(j in 36:40){
    file.copy(files2017_long[i], paste0('../data/out_753/newfiles/dummy_forcings/', 'f', j, '_', files2017[i]))
  }
}

# now list for force.prm
# hydro
hydro_files <- list.files('../data/out_753/newfiles/dummy_forcings/hydro/', full.names = F)
this_hydrofile <- '../data/out_753/newfiles/dummy_forcings/hydro_filenames.txt'
file.create(this_hydrofile)

for(i in 1:length(hydro_files)){
  cat(paste0('hd', i-1, '.name ../dummy_heatwave/hydro/', hydro_files[i]), file = this_hydrofile, append = T, '\n')
}

# salt
salt_files <- list.files('../data/out_753/newfiles/dummy_forcings/salt/', full.names = F)
this_saltfile <- '../data/out_753/newfiles/dummy_forcings/salt_filenames.txt'
file.create(this_saltfile)

for(i in 1:length(salt_files)){
  cat(paste0('Salinity', i-1, '.name ../dummy_heatwave/salt/', salt_files[i]), file = this_saltfile, append = T, '\n')
}

# temp
temp_files <- list.files('../data/out_753/newfiles/dummy_forcings/temp/', full.names = F)
this_tempfile <- '../data/out_753/newfiles/dummy_forcings/temp_filenames.txt'
file.create(this_tempfile)

for(i in 1:length(temp_files)){
  cat(paste0('Temperature', i-1, '.name ../dummy_heatwave/temp/', temp_files[i]), file = this_tempfile, append = T, '\n')
}
