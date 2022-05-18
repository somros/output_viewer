library(ReactiveAtlantis)
library(tidyverse)
library(ncdf4)
library(rbgm)
library(data.table)
library(lubridate)
library(sf)
library(RColorBrewer)

select <- dplyr::select

this.path <- 'C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data'
this.dir <- 'outputFolder_v033_01'

setwd(paste(this.path,this.dir,sep='/'))

nc.current  <- 'outputGOA033_test.nc'
nc.old      <- '../outputFolder_v027_01/outputGOA027_test.nc'
grp.csv     <- '../GOA_Groups_WHH.csv'
#grp.csv     <- '../GOA_Groups_annual.csv'
bgm.file    <- 'GOA_WGS84_V4_final.bgm'
cum.depths  <- c(0,30,100,200,500,1000,4000) ## This should be the cummulative depth of your model
## individual file
# compare(nc.current, nc.out.old = NULL, grp.csv, bgm.file, cum.depths)
# ## compare to previuos run
compare(nc.current, nc.old, grp.csv, bgm.file, cum.depths)

# predator-prey interactions from initial conditions
#prm.file    <- '../GOAbioparam_test_new.prm'
prm.file <- '../GOAbioparam_test_fixed.prm'
nc.initial  <- '../GOA_cb_new.nc'
feeding.mat(prm.file, grp.csv, nc.initial, bgm.file, cum.depths)

# predation output
biom        <- 'outputGOA033_testBiomIndx.txt'
diet.file   <- 'outputGOA033_testDietCheck.txt'
bio.age     <- 'outputGOA033_testAgeBiomIndx.txt' ## optional file. just if you want to check the predation by age
## Predation by Age
predation(biom, grp.csv, diet.file, bio.age)

diet.file   <- 'outputGOA015_testDietCheck.txt'
food.web(diet.file, grp.csv)
## optional you can explore the food web by polygon
# food.web(diet.file, grp.file, diet.file.bypol)
## diet.file.bypol Detailed diet check file, this can be obtained as an extra output from Atlantis "DetailedDietCheck.txt". To get this file from Atlantis turn on the option "flagdietcheck" on the Run.prm file.


# Biomasses ---------------------------------------------------------------

# can we get biomasses on day 73 and 146 from the nc output?

test <- nc_open(nc.current)
allv <- test$var
n.vars <- names(test$var[grepl('_N\\b', test$var)])

# make df with box number, area, total depth, layers, and layer thickness 
atlantis_box <- bgm.file %>% read_bgm() %>% box_sf()

lyr <- data.frame('dz'=cum.depths[-1],'layer'=0:5)

# we need to construct the depth layers in the correct order
make_layer <- function(z,cumdepths=cum.depths){
  these_layers <- findInterval(z,cumdepths)
  
  pad <- rep(0,(6-these_layers))
  lyr = c(pad, z, rev(lyr[lyr$dz<z,1])) # 0 thickness for non-existing layers
  lyr
}

wc <- atlantis_box %>%
  select(box_id, area, botz) %>%
  mutate(botz=-botz,
         depths = purrr::map(botz, make_layer)) %>%
  unnest(cols = depths) %>%
  mutate(layer = rep(0:5, 109)) %>%
  group_by(box_id,area) %>%
  mutate(dz = depths-lead(depths, default = 0)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(dz = ifelse(dz<0,0,dz)) %>%
  ungroup() %>%
  select(box_id, area, layer, dz)

# add a layer for sediment
sed <- atlantis_box %>%
  select(box_id, area) %>%
  mutate(layer = 6, dz = 1)

# bind wc and sed and get volume
all <- rbind(wc,sed) %>% 
  arrange(box_id, layer) %>%
  mutate(vol = area*dz)

# read nc slices and get biomass in t from concentration 

get_slice <- function(this_var, ts, template=all){
  
  this_var_atts <- allv[[which(names(allv)==this_var)]]
  
  if(this_var_atts$units == 'mg N m-2'){ # here 2D or 3D
    t1_conc <- ncvar_get(test, this_var)[,ts] # 1 = initial (Jan 1); 2 = 73 days (spring); 3 = 146 days (summer)
    
    this_biomass <- atlantis_box %>%
      select(box_id,area) %>%
      mutate(conc = t1_conc,
             biomass = conc*area*5.7*20*1e-9) %>% # to dry, to wet, to tons
      pull(biomass) %>%
      sum()
    
  } else {
    t1_conc <- ncvar_get(test, this_var)[,,ts] # 1 = initial (Jan 1); 2 = 73 days (spring); 3 = 146 days (summer)
    
    this_biomass <- data.frame(t(t1_conc)) %>% 
      set_names(0:6) %>% mutate(box_id=0:108) %>% 
      pivot_longer(cols=-box_id, names_to = 'layer', values_to = 'N') %>%
      mutate(layer=as.numeric(layer)) %>%
      left_join(template, by = c('box_id','layer')) %>%
      mutate(biomass = N*vol*5.7*20*1e-9) %>%
      pull(biomass) %>%
      sum()
  }
  
  return(this_biomass)
}

# plot time series
tttt <- list()
steps <- ncvar_get(test, 't')

for(i in 1:length(steps)){
  tttt[[i]] <- data.frame('t'=steps[i],'fg'=n.vars, 'biomass_t'=unlist(lapply(n.vars,get_slice,i)))
}

biom_ts <- rbindlist(tttt)

t1 <- biom_ts %>% filter(fg == 'Octopus_N')

plot(t1$t, t1$biomass_t)

# view these:
# turn ts to date at some stage
# 1: aggregate groups into guilds (or else too much clutter)
# 2: make area plots of t m-2
# 3: make area plots of percentages
# tie to atlantis grouptypes and look at the same for fish only
# have something to corroborate this broken down by group

# aggregate groups into guilds - 
#write.csv(data.frame('fg'=unique(biom_ts$fg)),'../fg.csv', row.names = F)
fg_key <- read.csv('../fg_to_guild.csv')
fg_goa <- read.csv('../GOA_Groups.csv')

goa_area <- (atlantis_box %>% filter(botz<0, boundary!=TRUE) %>% st_set_geometry(NULL)) %>% pull(area) %>% sum()/1000000

biom_ts <- biom_ts %>%
  left_join(fg_key, by = 'fg') %>%
  mutate(Name = gsub('_N','',fg), Time = as.POSIXct(t, origin = '1990-01-01 00:00:00', 'UTC')) %>%
  left_join((fg_goa %>% select(Name, GroupType)), by = 'Name')
  
# BY GUILD
# prepare for viewing
biom_ts_guilds <- biom_ts %>%
  group_by(Time, Guild) %>%
  summarise(Biomass_t_km2 = sum(biomass_t)/goa_area) %>%
  group_by(Time) %>%
  mutate(Biomass_percent = Biomass_t_km2/sum(Biomass_t_km2))

# get some new colors
colourCount <- length(unique(biom_ts_guilds$Guild))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

biom_ts_guilds %>%
  filter(Time<'2000-01-01') %>%
  ggplot() + 
  geom_area(aes(x=Time, y=Biomass_percent, fill = Guild))+
  scale_fill_manual(values=getPalette(colourCount))+
  theme_bw()

p <- biom_ts_guilds %>%
  filter(Time>'1990-01-01',Time<'1990-04-01') %>%
  mutate(Guild = fct_reorder(Guild, desc(Biomass_t_km2))) %>%
  ggplot()+
  geom_bar(aes(x=Guild,y=Biomass_t_km2,fill=Guild), position='dodge', stat='identity')+
  scale_y_continuous(breaks = seq(0,200,10), trans = 'log1p')+
  scale_fill_manual(values=getPalette(colourCount))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(x='',y='Log biomass t/km2', title = 'March 15 1990')
p
ggsave('../biomass_structure.png',p,width=8.5,height=7)

# look at fish groups only
biom_ts_fish <- biom_ts %>%
  filter(GroupType == 'FISH') %>%
  group_by(Time, fg) %>%
  summarise(Biomass_t_km2 = sum(biomass_t)/goa_area) %>%
  group_by(Time) %>%
  mutate(Biomass_percent = Biomass_t_km2/sum(Biomass_t_km2))

colourCount <- length(unique(biom_ts_fish$fg))

p <- biom_ts_fish %>%
  filter(Time>'1990-01-01',Time<'1990-04-01') %>%
  mutate(fg = fct_reorder(fg, desc(Biomass_t_km2))) %>%
  ggplot()+
  geom_bar(aes(x=fg,y=Biomass_t_km2,fill=fg), position='dodge', stat='identity')+
  #scale_y_continuous(breaks = seq(0,200,10), trans = 'log1p')+
  scale_fill_manual(values=getPalette(colourCount))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(x='',y='Biomass t/km2', title = 'March 15 1990')
p
ggsave('../biomass_structure_fish.png',p,width=8.5,height=7)

# # BY ATLANTIS GROUPTYPE
# biom_ts_gt <- biom_ts %>%
#   group_by(Time, GroupType) %>%
#   summarise(Biomass_t_km2 = sum(biomass_t)/goa_area) %>%
#   group_by(Time) %>%
#   mutate(Biomass_percent = Biomass_t_km2/sum(Biomass_t_km2))
# 
# # get some new colors
# colourCount <- length(unique(biom_ts_gt$GroupType))
# getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
# 
# biom_ts_gt %>%
#   filter(Time<'1995-01-01') %>%
#   ggplot() + 
#   geom_area(aes(x=Time, y=Biomass_percent, fill = GroupType))+
#   scale_fill_manual(values=getPalette(colourCount))+
#   theme_bw()

# barchart for one year with all species
ttt <- data.frame('fg'=n.vars, 'biomass_t'=unlist(lapply(n.vars,get_slice,2)))

p <- ttt %>% 
  mutate(fg = fct_reorder(fg, desc(biomass_t))) %>%
  ggplot()+
  geom_bar(aes(x=fg,y=biomass_t), stat='identity')+
  scale_y_continuous(trans = 'log1p')+
  #scale_fill_discrete(guide = guide_legend(ncol=2))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

p

ggsave('../biomass_snapshot.png', p, width = 9, height = 6)


# Other diagnostics -----------------------------------------------

# This is the biomass index (total)

biomidx <- 'outputGOA013_testBiomIndx.txt'

biomframe <- read.table(biomidx, sep=' ', header = T) %>%
  select(Time:DIN) %>%
  pivot_longer(cols = -Time, names_to = 'Code', values_to = 'BiomIdx_mt') %>%
  left_join((read_csv(grp.csv) %>% select(Code, Name)), by= 'Code')

horlines <- biomframe %>% filter(Time==0)

p <- biomframe %>%
  ggplot(aes(x=Time,y=BiomIdx_mt))+
  geom_line(size = 1)+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  geom_hline(data = horlines, aes(yintercept = BiomIdx_mt), color = 'red', size = 1)+
  theme_bw()+
  facet_wrap(~Name, scales='free', ncol = 6)
p

ggsave('biomplot.png', p, width = 8, height = 12)


# Light and Silica --------------------------------------------------------

# do we have problems with diatoms? check here

Si <- list()

for(i in 1:10){
  
  Si_conc <- ncvar_get(test, 'Si')[,,i]
  
  Si[[i]] <- data.frame(t(Si_conc)) %>% 
    set_names(0:6) %>% mutate(box_id=0:108) %>% 
    pivot_longer(cols=-box_id, names_to = 'layer', values_to = 'Si') %>%
    mutate(layer=as.numeric(layer)) %>%
    left_join(all, by = c('box_id','layer')) %>%
    mutate(ts = i)
  
}

Si <- rbindlist(Si)

Si %>% ggplot(aes(x=ts, y=Si, group = box_id))+
  geom_line()+geom_point()+theme_bw()+facet_wrap(~layer, scales='free')

# definitely a problem with Si. WHere is this even taken from? It is very large in the sediment and pretty low everywhere else

light <- list()

for(i in 2:15){
  
  light_conc <- ncvar_get(test, 'Light')[,,i]
  
  light[[i]] <- data.frame(t(light_conc)) %>% 
    set_names(0:6) %>% mutate(box_id=0:108) %>% 
    pivot_longer(cols=-box_id, names_to = 'layer', values_to = 'light') %>%
    mutate(layer=as.numeric(layer)) %>%
    left_join(all, by = c('box_id','layer')) %>%
    mutate(ts = i)
  
}

light <- rbindlist(light)

light %>% ggplot(aes(x=ts, y=light, group = box_id))+
  geom_line()+geom_point()+theme_bw()+facet_wrap(~layer, scales='free')
