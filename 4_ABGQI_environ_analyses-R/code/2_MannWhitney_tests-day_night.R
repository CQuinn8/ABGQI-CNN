# Purpose: use hourly ABGOQ values to perform Mann-Whitney analyses for day and night soundscape differences
# Requires: site_by_hour_ABGQIU_fscore_075.csv and S2L_site_geog-env_data.csv
# Outputs: Results 3.2.1
# Author: Colin Quinn <cq73@nau.edu>

# set working directory to home directory of repo
setwd('/published_repo/')

library(dplyr)
library(tidyr)
library(ggplot2)
source('support_functions/color_theme_fxs.R')
source('support_functions/functions.R')

# location for agg data and results
data_dir = '4_ABGQI_environ_analyses-R/data/'
df = read.csv(file = paste0(data_dir, "site_by_hour_ABGQIU_fscore_075.csv"))

# env data
env_df = read.csv(paste0(data_dir, 'S2L_site_geog-env_data.csv')) %>%
  select(-n_min)

# select useful vars
df = merge(x = df, y = env_df, by = 'site')
mod_df = data.frame('site' = df$site,
                    'Anthropophony' = df$Anthropophony, 
                    'Biophony' = df$Biophony, 
                    'Quiet' = df$Quiet,
                    'Geophony' = df$Geophony,
                    'Unidentified' = df$Unidentified,
                    'Interference' = df$Interference,
                    'HH' = df$HH,
                    'LULC' = df$LULC)

# rename LCLU
# structural order: UD, AB, HB, SH, RW, FO, FC
mod_df$LULC = factor(mod_df$LULC, levels = c("Urban/Developed",  
                                                       "Agriculture/Barren",   
                                                       "Herbaceous",  
                                                       "Shrubland",  
                                                       "Riparian/Wetland", 
                                                       "Oak/Hardwood Forest", 
                                                       "Conifer Forest"))

# add in day/night
mod_df = mod_df %>%
  mutate(DayNight = ifelse(HH < 5 | HH >= 20, 'Night', 'Day'))
mod_df$DayNight = factor(mod_df$DayNight, levels = c('Night','Day'))



######## Day vs Night Analysis #################
temp_df = mod_df %>%
  mutate(Anthropophony = Anthropophony * 100,
         Biophony = Biophony * 100,
         Geophony = Geophony * 100,
         Quiet = Quiet * 100)

# view ABGQIU Day vs Night box plots
temp_df %>%
  dplyr::select(Anthropophony, Biophony, Geophony, Quiet, Interference, Unidentified, DayNight) %>%
  gather(variable, value, -DayNight) %>%
  ggplot(aes(x = variable, y = value, fill = DayNight)) +
  geom_boxplot(notch = T, outlier.fill = 'grey', outlier.alpha = 0.1) +
  labs(x = '',
       y = 'Percent Present') +
  theme_Publication() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_Publication()

# Numeric value for ABGQ
temp_df %>%
     dplyr::select(Anthropophony, Biophony, Geophony, Quiet, DayNight) %>%
     group_by(DayNight) %>%
     summarise(across(where(is.numeric), median))

# MW-U tests
# Anthro
wilcox.test(Anthropophony ~ DayNight, data = temp_df, conf.int = T, exact = T)

# Bio
wilcox.test(Biophony ~ DayNight, data = temp_df, conf.int = T, exact = T)

# Geo
wilcox.test(Geophony ~ DayNight, data = temp_df, conf.int = T, exact = T)

# Quiet
wilcox.test(Quiet ~ DayNight, data = temp_df, conf.int = T, exact = T)

# Int
wilcox.test(Interference ~ DayNight, data = temp_df, conf.int = T, exact = T)

# U
wilcox.test(Unidentified ~ DayNight, data = temp_df, conf.int = T, exact = T)


######## Day vs Night + LULC Analysis #################
# LULC day/night faceted by sound 
temp_df %>%
  dplyr::select(Anthropophony, Biophony, Geophony, Quiet, DayNight, LULC) %>%
  gather(variable, value, -DayNight, -LULC) %>%
  ggplot(aes(x = variable, y = value, fill = DayNight)) +
  geom_boxplot(notch = T, outlier.alpha = 0.2) +
  labs(x = '',
       y = 'Percent Present') +
  facet_wrap(~LULC) +
  theme_Publication() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  scale_fill_Publication()

# Day/night sounds faceted by LULC
temp_df %>%
  dplyr::select(Anthropophony, Biophony, Geophony, Quiet, DayNight, LULC) %>%
  gather(variable, value, -DayNight, -LULC) %>%
  ggplot(aes(x = LULC, y = value, fill = DayNight)) +
  geom_boxplot(notch = T, outlier.alpha = 0.2, alpha = 0.7) +
  labs(x = '',
       y = 'Percent Present') +
  facet_wrap(~variable) +
  theme_Publication() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  scale_fill_Publication()
