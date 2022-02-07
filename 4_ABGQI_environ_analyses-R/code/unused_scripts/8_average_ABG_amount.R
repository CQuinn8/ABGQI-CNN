# Purpose: 
# Requires: 
# Outputs: 
# Author: Colin Quinn <cq73@nau.edu>

# set working directory to home directory of repo
setwd('/published_repo/')

library(dplyr)
library(tidyr)
library(ggplot2)
source('./color_theme_fxs.R')

# location for agg data and results
data_dir = './4_ABGQI_environ_analyses-R/data/'
df = read.csv(file = paste0(data_dir, "site_by_hour_ABGQIU_fscore_075.csv"))

# summarize hourly variation and mean sounds
# mean
colMeans(df[,c(-1,-2)])
# St.Dev
apply(df[,3:8], 2, function(x) sd(x))


# amount of total sound preds
df %>%
  #filter(between(HH, 5, 20)) %>% # daytime
  summarise(across(where(is.numeric), mean))

