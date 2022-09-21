# biomass trend plots from text files - comparison
library(tidyverse)
setwd('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/code/')

file1 <- read.table('../data/out_248/outputGOA0248_testBiomIndx.txt', sep = ' ', header = T)
file2 <- read.table('../data/out_251/outputGOA0251_testBiomIndx.txt', sep = ' ', header = T)
groups <- read.csv('../data/GOA_Groups.csv')


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
ggsave('compare_csv.png',p,width=8,height = 8)




