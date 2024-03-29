# Alberto Rovellini
# 11/14/2022
# Code to visualise output CSV files

# library(tidyverse)
# library(RColorBrewer)
# library(readxl)

# Total biomass per year --------------------------------------------------

all_files <- list.files()
dat <- read.table(paste0(all_files[grepl('testBiomIndx.txt',all_files)]), header = T)
groups <- read.csv(grp.csv)
groups <- groups %>% select(Code, Name, LongName, GroupType)
codes <- groups %>% pull(Code)

# # read B0 information from stock assessment
# sa_t3 <- read_excel('GOA MSY estimates tables.xlsx', sheet = 'Tier 3', range = 'A3:L19')
# sa_t4 <- read_excel('GOA MSY estimates tables.xlsx', sheet = 'Tier 4 and 5', range = 'A3:I10')
# 
# # calculate B0
# # BUT: B0 in this table refers to FSSB I am hoping?? If not we have a problem.
# sa_t3 <- sa_t3 %>%
#   select(Stock, `B40%`) %>%
#   mutate(B0 = `B40%` / 40 * 100) %>%
#   select(Stock, B0) %>%
#   mutate(Tier = '3')
# 
# sa_t4 <- sa_t4 %>%
#   select(`Stock/Stock complex`, `Average Biomass`, `Stock depletion`) %>%
#   mutate(B0 = `Average Biomass` / `Stock depletion`) %>%
#   select(`Stock/Stock complex`, B0) %>%
#   set_names(c('Stock','B0')) %>%
#   mutate(Tier = '4,5')
# 
# sa <- rbind(sa_t3, sa_t4)
# 
# # map to atlantis codes
# sa$Code <- c('POL','COD','SBF','FFS','FFS','FFS','FFS','FFD','REX','REX','ATF','FHS','POP','RFS','RFS','RFP','FFS','RFS','RFD','RFD','THO','DOG','SKB')
# 
# # sum across groups
# sa <- sa %>% group_by(Code) %>% summarise(B0 = sum(B0))

dat_long <- dat %>%
  select(Time, all_of(codes)) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'Biomass') %>%
  left_join(groups, by = 'Code')# %>%
  #left_join(sa, by = 'Code')

# get initial biomasses
init <- dat_long %>%
  #filter(GroupType %in% c('FISH','SHARK','MAMMAL','BIRD')) %>%
  filter(Time == 0)

biom <- dat_long %>%
  #filter(GroupType %in% c('FISH','SHARK','MAMMAL','BIRD')) %>%
  ggplot()+
  geom_line(aes(x = Time, y = Biomass), linewidth = 1)+
  geom_hline(data = init, aes(yintercept = Biomass), color = 'green', linewidth = 1, linetype = 'dashed')+
  #geom_hline(aes(yintercept = B0), color = 'red')+
  theme_bw()+
  labs(x = 'Year', y = 'Biomass (t)')+
  facet_wrap(~LongName, scales = 'free', ncol = 4)

ggsave('biom.png', biom, width = 12, height = 18)

# and relative
relbiom <- dat_long %>%
  #filter(GroupType %in% c('FISH','SHARK','MAMMAL','BIRD')) %>%
  group_by(Code) %>%
  mutate(Rel = Biomass/Biomass[1]) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(x = Time, y = Rel), linewidth = 1)+
  geom_hline(yintercept = 1, color = 'green', linewidth = 1, linetype = 'dashed')+
  #geom_hline(aes(yintercept = B0), color = 'red')+
  theme_bw()+
  labs(x = 'Year', y = 'Relative biomass')+
  facet_wrap(~LongName, scales = 'free', ncol = 4)

ggsave('biom_rel.png', relbiom, width = 12, height = 18)

# # Biomass by age class ----------------------------------------------------
# 
# dat_age <- read.table(paste0('../data/out_', this_run, '/outputGOA0', this_run, '_testAgeBiomIndx.txt'), sep = ' ', header = T)
# 
# dat_age_long <- dat_age %>%
#   pivot_longer(-Time, names_to = 'Code_cohort', values_to = 'Biomass') %>%
#   separate(Code_cohort, c('Code', 'Cohort')) %>%
#   left_join(groups, by = 'Code') %>%
#   mutate(Time = Time / 365)
# 
# dat_age_long %>%
#   filter(GroupType == 'FISH') %>%
#   ggplot()+
#   geom_line(aes(x = Time, y = Biomass, color = Cohort))+
#   theme_bw()+
#   labs(x = 'Year', y = 'Biomass (t)')+
#   facet_wrap(~Name, scales = 'free')
# 
# # Biomass structure --------------------------------------------------------
# 
guilds <- read.csv('../fg_to_guild.csv')
guilds$fg <- gsub('_N', '', guilds$fg)

colourCount <- length(unique(guilds$Guild))
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

biomstruct <- dat_long %>%
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

ggsave('biomass_structure.png', biomstruct, width = 10, height = 5)

# get biom structure but exclude detritus and bacteria
nondet <- setdiff(codes, c("BB",  "PB",  "DC",  "DL",  "DR"))

biomstruct2 <- dat_long %>%
  filter(Code %in% nondet) %>%
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

ggsave('biomass_structure_no_detritus.png', biomstruct2, width = 10, height = 5)

#   
# # Compare -----------------------------------------------------------------
# 
# file1 <- read.table('../data/out_399/outputGOA0399_testBiomIndx.txt', sep = ' ', header = T)
# file2 <- read.table('../data/out_400/outputGOA0400_testBiomIndx.txt', sep = ' ', header = T)
# 
# file1_l <- file1 %>%
#   pivot_longer(-Time, names_to = 'Group', values_to = 'Biomass (t)') %>%
#   mutate(Run = 'Previous')
# 
# file2_l <- file2 %>%
#   pivot_longer(-Time, names_to = 'Group', values_to = 'Biomass (t)') %>%
#   mutate(Run = 'Current')
# 
# file_joint <- rbind(file1_l, file2_l)
# 
# # add group names
# file_joint <- file_joint %>%
#   left_join(groups %>% select(Code,Name), by = c('Group' = 'Code'))
# 
# #plot
# p <- file_joint %>%
#   filter(Group %in% c('POL','COD','ATF','HAL','POP','SBF')) %>%
#   ggplot(aes(x = Time, y = `Biomass (t)`, color = Run))+
#   scale_color_manual(values = c('blue','orange'))+
#   geom_line(size = 2)+
#   theme_bw()+
#   facet_wrap(~Name, ncol = 2, scales = 'free_y')
# 
# p
# 
# ##ggsave('compare_csv.png',p,width=8,height = 8)


# Catch -------------------------------------------------------------------

catch <- read.table(paste0(all_files[grepl('testCatch.txt',all_files)]), header = T)

catch_long <- catch %>%
  pivot_longer(-Time) %>%
  filter(name %in% codes,
         Time > 0)
catch_t1 <- catch_long %>%
  filter(Time == 365)

# plot
catch_plot <- ggplot(catch_long, aes(x = Time, y = value))+
  geom_line(linewidth = 1.2)+
  geom_hline(data = catch_t1, aes(yintercept = value), linetype = 'dashed', color = 'red', linewidth = 1.2)+
  theme_bw()+
  labs(x ='', y='Catch (t)', main = 'Total catch')+
  facet_wrap(~name, ncol = 6, scales = 'free')
catch_plot
ggsave('catch_total.png', catch_plot, width = 12, height = 10)


# relative
rel_catch_plot <- catch_long %>%
  group_by(name) %>%
  mutate(relcatch = value/value[1]) %>%
  ungroup() %>%
  ggplot(aes(x = Time, y = relcatch))+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', linewidth = 1.2)+
  theme_bw()+
  labs(x ='', y='Relative catch')+
  facet_wrap(~name, ncol = 6, scales = 'free')
ggsave('rel_catch_total.png', rel_catch_plot, width = 12, height = 10)

# catch by fishery
cbf <- read.table(paste0(all_files[grepl('testCatchPerFishery.txt',all_files)]), header = T)

cbf_long <- cbf %>%
  pivot_longer(-c(Time,Fishery)) %>%
  filter(name %in% codes,
         Time > 0)
# check that there is no catch from anything that is not background
cbf_long %>%
  filter(Fishery != 'background') %>%
  pull(value) %>%
  sum() # 0 - good

# # plot and see that it is the same plot as above
# # for now not useful as we only have 1 fishery
# 
# cbf_plot <- cbf_long %>%
#   filter(Fishery == 'background') %>%
#   ggplot(aes(x = Time, y = value))+
#   geom_line(linewidth = 1.2)+
#   geom_hline(data = catch_t1, aes(yintercept = value), linetype = 'dashed', color = 'red', linewidth = 1.2)+
#   theme_bw()+
#   labs(x ='', y='Catch (t)', main = 'Catch by fishery')+
#   facet_wrap(~name, ncol = 6, scales = 'free')
# cbf_plot
# ggsave('catch_by_fishery.png', cbf_plot, width = 12, height = 10)

