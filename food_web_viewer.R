# View food web

library(tidyverse)
library(igraph)
library(RColorBrewer)

select <- dplyr::select

this.path <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/'
this.dir <- 'outputFolder_v042_01'

setwd(paste0(this.path, this.dir))

# read data
dietcheck <- read.table('outputGOA042_testDietCheck.txt', header = T)
biomindex <- read.table('outputGOA042_testBiomIndx.txt', header = T)
fg <- read.csv('../GOA_Groups.csv', header = T) %>% select(Code,Name)
fg_key <- read.csv('../fg_to_guild.csv') %>% mutate(fg = str_replace(fg, '_N', ''))

# pick time step in days
time.step <- 365
# pick threshold value of for trophic connections
thresh <- 0.01 # 1% of consumption for the group minimum

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

# make two dataframes for the graph_from_data_frame() function
# make weights for vertex size based on biomass at that time step
# will have to be on log scale or we won't be able to see much
vertex_weights <- biomindex %>%
  filter(Time == time.step) %>%
  select(KWT:DR) %>%
  pivot_longer(everything()) %>%
  mutate(Weight = 10*log1p(value)/max(log1p(value)), # log scale and standardize, the *10 seems to work OK with the graphs
         Weight_3D = log1p(value)) %>% 
  left_join(fg, by = c('name'='Code')) %>%
  left_join(fg_key, by = c('Name'='fg'))
  
edge_weights <- dietall %>%
  pivot_longer(-Predator, names_to = 'to', values_to = 'dietprop') %>%
  rename(from = Predator) %>%
  mutate(dietprop = dietprop*10) %>% # 10 is tentative, just set it to whatever aid visualisation in the graph
  filter(dietprop > thresh) 

g <- graph_from_data_frame(edge_weights, directed=TRUE, vertices=vertex_weights)

# set up colors
nguilds <- nlevels(as.factor(vertex_weights$Guild))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
colframe <- data.frame('guild'=1:nguilds, 'col'=getPalette(nguilds))
colkey <- data.frame('guild' = as.numeric(as.factor(vertex_weights$Guild))) %>%
  left_join(colframe, 'guild')

plot(g,
     vertex.size = vertex_weights$Weight,
     vertex.shape = 'sphere',
     vertex.color = colkey$col,
     vertex.label = vertex_weights$Name,
     vertex.label.cex=1.5,
     edge.width = E(g)$dietprop,
     edge.arrow.size = 0.1,
     layout = layout.sphere, 
     main="GOA food web"
     )

l <- layout_in_circle(g)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(g,
     vertex.size = vertex_weights$Weight,
     vertex.shape = 'sphere',
     vertex.color = colkey$col,
     vertex.label = vertex_weights$Name,
     vertex.label.cex=1.5,
     edge.width = E(g)$dietprop,
     edge.arrow.size = 0.5,
     layout = l, 
     main="GOA food web"
)


# 3D interactive version
library(networkD3)
library(htmlwidgets)

# p <- simpleNetwork(edge_weights, height="10000px", width="10000px")
# p
# saveWidget(p, file = 'networkInteractive2.html')

nodes_frame <- vertex_weights %>%
  mutate(idx = 0:(nrow(vertex_weights)-1))

key <- data.frame('fg'=fg$Code, 'idx'=0:(nrow(fg)-1))

links_frame <- edge_weights %>%
  left_join(key, by = c('from'='fg')) %>%
  left_join(key, by = c('to' = 'fg')) %>%
  select(idx.x, idx.y, dietprop) %>%
  set_names(c('from','to','dietprop'))

# better now
p <- forceNetwork(Links = links_frame, 
             Nodes = nodes_frame, 
             Source = "from",
             Target = "to", 
             Value = "dietprop", 
             NodeID = "Name", 
             Nodesize = 'Weight_3D', 
             Group = "Guild", 
             fontSize = 18,
             opacity = 0.8, 
             opacityNoHover = 0.1,
             charge = -300,
             legend = TRUE,
             zoom = TRUE,
             arrows = TRUE#,
             #main = paste("Atlantis GOA food web at time", time.step, "days")
             )
p

saveWidget(p, file = 'networkInteractive2.html')
