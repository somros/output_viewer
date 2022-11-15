# Alberto Rovellini
# 11/14/2022
# Code to visualise output CSV files

library(tidyverse)
library(RColorBrewer)

setwd('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/code/')

this_run <- 414

# Total biomass per year --------------------------------------------------

dat <- read.table(paste0('../data/out_', this_run, '/outputGOA0', this_run, '_testBiomIndx.txt'), sep = ' ', header = T)
groups <- read.csv('../data/GOA_Groups.csv')
groups <- groups %>% select(Code, Name, GroupType)
codes <- groups %>% pull(Code)

dat_long <- dat %>%
  select(Time, all_of(codes)) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'Biomass') %>%
  left_join(groups, by = 'Code')

dat_long %>%
  filter(GroupType == 'FISH') %>%
  ggplot()+
  geom_line(aes(x = Time, y = Biomass))+
  theme_bw()+
  labs(x = 'Year', y = 'Biomass (t)')+
  facet_wrap(~Name, scales = 'free', ncol = 7)

# Biomass by age class ----------------------------------------------------

dat_age <- read.table(paste0('../data/out_', this_run, '/outputGOA0', this_run, '_testAgeBiomIndx.txt'), sep = ' ', header = T)

dat_age_long <- dat_age %>%
  pivot_longer(-Time, names_to = 'Code_cohort', values_to = 'Biomass') %>%
  separate(Code_cohort, c('Code', 'Cohort')) %>%
  left_join(groups, by = 'Code') %>%
  mutate(Time = Time / 365)

dat_age_long %>%
  filter(GroupType == 'FISH') %>%
  ggplot()+
  geom_line(aes(x = Time, y = Biomass, color = Cohort))+
  theme_bw()+
  labs(x = 'Year', y = 'Biomass (t)')+
  facet_wrap(~Name, scales = 'free')

# Biomass structure --------------------------------------------------------

guilds <- read.csv('../fg_to_guild.csv')
guilds$fg <- gsub('_N', '', guilds$fg)

colourCount <- length(unique(guilds$Guild))
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

dat_long %>%
  left_join(guilds, by = c('Name'= 'fg')) %>% # add guilds
  group_by(Time, Guild) %>%
  summarise(Biomass_tot = sum(Biomass)) %>%
  group_by(Time) %>%
  mutate(Prop = Biomass_tot/sum(Biomass_tot)) %>%
  ggplot()+
  geom_bar(aes(x = Time, y = Prop, fill = Guild), stat = 'identity', position = 'stack')+
  scale_fill_manual(values=getPalette(colourCount))+
  theme_bw()+
  labs(x = 'Year', y = 'Biomass proportion')
  
# Compare -----------------------------------------------------------------

file1 <- read.table('../data/out_399/outputGOA0399_testBiomIndx.txt', sep = ' ', header = T)
file2 <- read.table('../data/out_400/outputGOA0400_testBiomIndx.txt', sep = ' ', header = T)

file1_l <- file1 %>%
  pivot_longer(-Time, names_to = 'Group', values_to = 'Biomass (t)') %>%
  mutate(Run = 'Previous')

file2_l <- file2 %>%
  pivot_longer(-Time, names_to = 'Group', values_to = 'Biomass (t)') %>%
  mutate(Run = 'Current')

file_joint <- rbind(file1_l, file2_l)

# add group names
file_joint <- file_joint %>%
  left_join(groups %>% select(Code,Name), by = c('Group' = 'Code'))

#plot
p <- file_joint %>%
  filter(Group %in% c('POL','COD','ATF','HAL','POP','SBF')) %>%
  ggplot(aes(x = Time, y = `Biomass (t)`, color = Run))+
  scale_color_manual(values = c('blue','orange'))+
  geom_line(size = 2)+
  theme_bw()+
  facet_wrap(~Name, ncol = 2, scales = 'free_y')

p

##ggsave('compare_csv.png',p,width=8,height = 8)
