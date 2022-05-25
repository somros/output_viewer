# Make a chord diagram of the food web
library(tidyverse)
library(circlize)
library(RColorBrewer)

select <- dplyr::select

this.path <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/'
this.dir <- 'outputFolder_v042_01'

setwd(paste0(this.path, this.dir))

# read data
dietcheck <- read.table('outputGOA042_testDietCheck.txt', header = T)
biomindex <- read.table('outputGOA042_testBiomIndx.txt', header = T)
fg <- read.csv('../GOA_Groups.csv', header = T) %>% select(Code,Name)

# pick time step in days
time.step <- 365
# pick threshold value of for trophic connections
thresh <- 0.25 # 

# need to collapse the cohorts, for now irrespective of life stages
dietshort <- dietcheck %>%
  group_by(Time, Predator) %>%
  summarise(across(KWT:DR, mean)) %>%
  ungroup() %>%
  filter(Time == time.step) %>% # select one time step
  select(-Time)

# we need to make this a square matrix, but the rows do not include producers
# add rows for all missing producers and detritus, and fill them with 0's
producers <- setdiff(names(dietshort)[-1], dietshort$Predator)
t <- matrix(0, ncol = (ncol(dietshort)-1), nrow = length(producers)) # make a matrix of 0's
prod_diet <- data.frame(producers,t) %>%
  set_names(names(dietshort))

# now paste, and order the rows same as the columns, then assign rownames, and remove first column
dietall <- rbind(dietshort, prod_diet) %>%
  arrange(factor(Predator, levels = names(dietshort))) 

dietall$Predator <- fg$Name
colnames(dietall) <- c('Predator', fg$Name)

dietall[dietall < thresh] <- 0

t <- dietall %>% column_to_rownames("Predator") %>% as.matrix()

png('chord.png', width = 15, height = 15, units = 'in', res = 600)

par(cex=1.5, mar = c(0,0,0,0))

chordDiagram(t, 
             directional = 1,
             link.arr.type = "big.arrow",
             annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(t))))))
# we go back to the first track and customize sector labels
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA
)

dev.off()