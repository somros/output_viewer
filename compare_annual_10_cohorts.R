# compare biominxed between annual and 10 cohorts
library(tidyverse)

setwd('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data')

biom_annual <- read.table('outputFolder_v018_01/outputGOA018_testBiomIndx.txt',sep = ' ',header = T)
biom_10 <- read.table('outputFolder_v023_01/outputGOA023_testBiomIndx.txt',sep = ' ',header = T)

# long
biom_annual_long <- biom_annual %>% 
  select(Time:DIN) %>%
  pivot_longer(-Time, names_to = 'Group', values_to = 'Biomass (t)') %>%
  mutate(Type = 'Annual cohorts')

biom_10_long <- biom_10 %>% 
  select(Time:DIN) %>%
  pivot_longer(-Time, names_to = 'Group', values_to = 'Biomass (t)') %>%
  mutate(Type = '10 cohorts')

#all
biom_long <- rbind(biom_annual_long,biom_10_long)

# look
biom_long %>%
  ggplot(aes(x=Time, y=`Biomass (t)`, color = Type))+
  geom_line(size = 2)+
  scale_color_manual(values = c('red','black'))+
  theme_bw()+
  facet_wrap(~Group, scales = 'free')
