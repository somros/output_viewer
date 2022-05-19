# View food web

library(tidyverse)
library(igraph)
library(RColorBrewer)
library(networkD3)
library(htmlwidgets)

select <- dplyr::select

make_foodweb_3D <- function(dietcheck.file, biomindex.file, groups.file, guilds.file = NULL, timestep, thresh){
  
  # read data
  dietcheck <- read.table(dietcheck.file, header = T)
  biomindex <- read.table(biomindex.file, header = T)
  fg <- read.csv(groups.file, header = T) %>% select(Code,Name)
  key <- data.frame('fg'=fg$Code, 'idx'=0:(nrow(fg)-1))
  
  if(!is.null(guilds.file)){
    fg_key <- read.csv(guilds.file) %>% mutate(fg = str_replace(fg, '_N', ''))
  }
  
  # collapse the cohorts, for now irrespective of life stages
  dietshort <- dietcheck %>%
    group_by(Time, Predator) %>%
    summarise(across(KWT:DR, mean)) %>%
    ungroup() %>%
    filter(Time == timestep) %>% # select one time step
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
  
  # make two dataframes for the graph_from_data_frame() function
  # make weights for vertex size based on biomass at that time step
  # will have to be on log scale or we won't be able to see much
  nodes_frame <- biomindex %>%
    filter(Time == timestep) %>%
    select(KWT:DR) %>%
    pivot_longer(everything()) %>%
    mutate(Weight = log1p(value)) %>% 
    left_join(fg, by = c('name'='Code')) %>%
    mutate(idx = 0:(nrow(.)-1))
  
  if(!is.null(guilds.file)){
    nodes_frame <- nodes_frame %>% left_join(fg_key, by = c('Name'='fg'))
  } else {
    nodes_frame <- nodes_frame %>% mutate(Guild = NA)
  }
  
  links_frame <- dietall %>%
    pivot_longer(-Predator, names_to = 'to', values_to = 'dietprop') %>%
    rename(from = Predator) %>%
    mutate(dietprop = dietprop*10) %>% # 10 is tentative, just set it to whatever aid visualisation in the graph
    filter(dietprop > thresh) %>%
    left_join(key, by = c('from'='fg')) %>%
    left_join(key, by = c('to' = 'fg')) %>%
    select(idx.x, idx.y, dietprop) %>%
    set_names(c('from','to','dietprop'))
  
  
  # plot
  p <- forceNetwork(Links = links_frame, 
                    Nodes = nodes_frame, 
                    Source = "from",
                    Target = "to", 
                    Value = "dietprop", 
                    NodeID = "Name", 
                    Nodesize = 'value', 
                    radiusCalculation = JS("Math.log(d.nodesize)+6"),
                    Group = "Guild", 
                    fontSize = 18,
                    opacity = 0.8, 
                    opacityNoHover = 0.1,
                    charge = -500,
                    legend = TRUE,
                    zoom = TRUE,
                    arrows = TRUE#,
                    #main = paste("Atlantis GOA food web at time", time.step, "days")
  )
  
  saveWidget(p, file = 'foodWebInteractive.html')
  
}

this.dietcheck <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/outputFolder_v023_02/outputGOA023_testDietCheck.txt'
this.biomindex <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/outputFolder_v023_02/outputGOA023_testBiomIndx.txt'
this.groups <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/GOA_Groups.csv'
this.guilds <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/fg_to_guild.csv'

make_foodweb_3D(
  
  dietcheck.file = this.dietcheck, # this is the dietcheck.txt file from your atlantis run
  biomindex.file = this.biomindex, # this is the biomindex.txt file from your atlantis run
  groups.file = this.groups, # this is your groups.csv file
  guilds.file = this.guilds, # optional file to visualise colors by trophic guild or similar
  timestep = 365, # this is one of the values in the 'Time' column of biomindex and dietcheck
  thresh = 0.01 # the smallest trophic interaction you want to visualise
  
)