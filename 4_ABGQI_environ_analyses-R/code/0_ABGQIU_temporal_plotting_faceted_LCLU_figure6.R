# Purpose: use aggregated CNN predictions to plot temporal and by-site patterns in ABGQIU (Figure 6)
# Requires: site_by_hour_ABGQIU_fscore_075.csv and S2L_site_geog-env_data.csv
# Outputs: figure 6 in manuscript; Section 3.2
# Author: Colin Quinn <cq73@nau.edu>

# set working directory to home directory of repo
setwd('/published_repo/')

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
source('support_functions/color_theme_fxs.R')
data_dir = '4_ABGQI_environ_analyses-R/data/'
df = read.csv(file = paste0(data_dir, "site_by_hour_ABGQIU_fscore_075.csv"))


################### BY HOUR PLOTTING aggregated by LCLU ###################
lulc = read.csv(paste0(data_dir, 'S2L_site_geog-env_data.csv'))
joined_df = merge(x = df, y = lulc, by = 'site') %>%
  select(-c(n_min, nearRoad_m, Recorder, YY, MM, firstDay, firstDate))


# data organization - should yield 6 classes * 7 LCLUs * 24 hrs = 1,008 rows
subset_df = joined_df %>%
  group_by(HH, LULC) %>%
  dplyr::summarise(across(where(is.numeric), mean)) %>% # mean by hour
  gather(variable, value, -HH, -LULC)
# relevel to manuscript level order
subset_df$variable = factor(subset_df$variable, levels = c('Anthropophony', 'Biophony', 'Quiet', 'Geophony', 'Interference', 'Unidentified'))

# plot each label on a new plot from 0-24 hours % faceted by LCLU
# if you receive a font error when plotting, use this to try and debug
# windowsFonts()
# windowsFonts("helvetica" = windowsFont("Helvetica")) 
(ggToD_LCLU_facet = 
    subset_df %>% 
    mutate(value = value*100) %>% 
    ggplot(aes(x = HH, y = value)) +
      scale_colour_Publication() +
      geom_line(aes(colour = variable), size = 1.2, alpha = 0.9) +
      theme_Publication() + # toggle this if your machine does not have the font required
      theme(legend.position = "top",
            legend.title = element_blank()) +
      scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c("0%", '20%', '40%', '60%')) +
      labs(title = "Percent of time present sound is predicted present for n = 746 sites",
           subtitle = '* = Unidentified determined by when all other labels are absent',
           x = "Hour of Day [0-24]",
           y = paste0("Percent present")) +
      facet_wrap(~LULC)) # facet based on ABGOQ


# ggsave(plot = ggToD_LCLU_facet,  
#        filename = "4_ABGQI_environ_analyses-R/results/figure6.png", 
#        width = 8, height = 8, units = "in")


# Summarize hourly patterns in soundscape classes
# mean
colMeans(df[,c(-1,-2)])
# St.Dev
apply(df[,3:8], 2, function(x) sd(x))