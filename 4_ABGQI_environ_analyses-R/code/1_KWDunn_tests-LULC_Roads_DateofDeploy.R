# Purpose: use aggregated ABGOQ values to perform kruskal wallis and dunn test analyses for LULC patterns, Road Group patterns differences
# Requires: site_avg_ABGQIU_fscore_075_daytime.csv and S2L_site_geog-env_data.csv
# Outputs: figure 7, figure 8, Results:3.2.2, 3.2.3, 3.2.4
# Author: Colin Quinn <cq73@nau.edu>

# set working directory to home directory of repo
setwd('/published_repo/')

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstatix) # Kruskal Wallis
source('support_functions/color_theme_fxs.R')
source('support_functions/functions.R')

# location for agg data and results
data_dir = '4_ABGQI_environ_analyses-R/data/'
df = read.csv(file = paste0(data_dir,"site_avg_ABGQIU_fscore_075_daytime.csv"))

# env data
env_df = read.csv(paste0(data_dir, 'S2L_site_geog-env_data.csv')) %>%
  select(-n_min)

######## PROCESS DATA ##################
# create grouped road dist var (0-100, 100-200, 200-300, 300-400, 400-500, 500+)
grouped_road = env_df %>%
  select(nearRoad_m, site) %>%
  mutate('RoadGroup' = case_when(nearRoad_m < 100 ~ '0-99',
                                 nearRoad_m >= 100 & nearRoad_m < 200 ~ '100-199',
                                 nearRoad_m >= 200 & nearRoad_m < 300 ~ '200-299',
                                 nearRoad_m >= 300 & nearRoad_m < 400 ~ '300-399',
                                 nearRoad_m >= 400 & nearRoad_m < 500 ~ '400-499', 
                                 nearRoad_m >= 500 & nearRoad_m < 600 ~ '500-599',
                                 nearRoad_m >= 600 & nearRoad_m < 700 ~ '600-699',
                                 nearRoad_m >= 700 & nearRoad_m < 800 ~ '700-799',
                                 nearRoad_m >= 800 & nearRoad_m < 900 ~ '800-899',
                                 nearRoad_m >= 900 & nearRoad_m < 1000 ~ '900-999',
                                 nearRoad_m >= 1000 ~ '>1000')) %>% 
  dplyr::select(!nearRoad_m)

# merge to environ df and ensure levels
env_df = merge(x = env_df, y = grouped_road, by = 'site')
env_df$RoadGroup = factor(env_df$RoadGroup, levels = c('0-99','100-199','200-299','300-399','400-499',
                                                       '500-599','600-699','700-799','800-899','900-999','>1000'))

# Join dataframes - ensure LULC levels
joined_df = merge(x = df, y = env_df, by = 'site')
joined_df$LULC = factor(joined_df$LULC, levels = c("Urban/Developed",  
                                                   "Agriculture/Barren",   
                                                   "Herbaceous",  
                                                   "Shrubland",  
                                                   "Riparian/Wetland", 
                                                   "Oak/Hardwood Forest", 
                                                   "Conifer Forest"), ordered = T)


######## LULC KW and Dunn ANAYSIS ######
# Results section 3.2.3 Daytime LULC stratification
# Kruskal Wallis: alternative for non-parametric one-way ANOVA when assumptions are not met
temp_df = joined_df %>%
  mutate(Anthropophony = Anthropophony * 100,
         Biophony = Biophony * 100,
         Geophony = Geophony * 100,
         Quiet = Quiet * 100)

# Anthro ***Significant***
kruskal.test(Anthropophony ~ LULC, data = temp_df)
Dunn_anthro = temp_df %>%
  dplyr::select(Anthropophony, LULC) %>%
  dunn_test(Anthropophony ~ LULC, p.adjust.method = "bonferroni") 

# Bio ***Significant***
kruskal.test(Biophony ~ LULC, data = temp_df)
Dunn_bio = temp_df %>%
  dplyr::select(Biophony, LULC) %>%
  dunn_test(Biophony ~ LULC, p.adjust.method = "bonferroni") 

# Geo ***NOT SIGNIFICANT*** 
kruskal.test(Geophony ~ LULC, data = temp_df)

# Quiet ***Significant***
kruskal.test(Quiet ~ LULC, data = temp_df)
Dunn_quiet = temp_df %>%
  dplyr::select(Quiet, LULC) %>%
  dunn_test(Quiet ~ LULC, p.adjust.method = "bonferroni") 

# visualize tests for Anthro, Quiet - Figure 7
# Anthro
Dunn_anthro = Dunn_anthro %>% add_xy_position(x = "LULC")
(ggAnthro = ggboxplot(temp_df, x = "LULC", y = "Anthropophony", 
                      add = "jitter", notch = TRUE,
                      add.params = list("jitter", color = "black", alpha = 0.3),
                      legend.title = "", xlab = FALSE, ylab = "Percent Present", main = "Anthropophony Dunn Test LCLU") +
  stat_pvalue_manual(Dunn_anthro, hide.ns = TRUE, bracket.nudge.y = 25) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), labels = c("0%", '25%', '50%', '75%', '100%')) +
  rotate_x_text(angle = 45))

# Quiet
Dunn_quiet = Dunn_quiet %>% add_xy_position(x = "LULC")
(ggQuiet = ggboxplot(temp_df, x = "LULC", y = "Quiet", 
                   #color = "LULC",  palette = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99"),
                   add = "jitter", notch = TRUE,
                   add.params = list("jitter", color = "black", alpha = 0.3),
                   legend.title = "", xlab = FALSE, ylab = "Percent Present", main = "Quiet Dunn Test LCLU") +
    stat_pvalue_manual(Dunn_quiet, hide.ns = TRUE) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100), labels = c("0%", '25%', '50%', '75%', '100%')) +
    rotate_x_text(angle = 45))



######## DEPLOYMENT DATE ANAYSIS #######
# Results: 3.2.2 Annual and date of deployment differences (not included)
temp_df = joined_df %>%
  dplyr::select(firstDate, YY)

# firstDate | YY : do not have equal var or means, are roughly normal/flat
# Kruskal Wallis: alternative for non-parametric one-way ANOVA when assumptions are not met
kruskal.test(firstDate ~ YY, data = temp_df)
# p-value = 2.2e-16 therefore treatment groups are significantly different

# Dunn test following KW test to see WHICH groups differ
(tempDunn = temp_df %>%
  dunn_test(firstDate ~ YY, p.adjust.method = "bonferroni"))
# based on pairwise: all deployment years are significantly different from each other


######## ANNUAL ANAYSIS #######
# Results: 3.2.2 Annual differences in sound for overlapping deployment date range
temp_df = joined_df %>%
  dplyr::select(firstDate, YY, Anthropophony, Biophony, Quiet) %>%
  mutate(firstDate = ymd(firstDate),
         dayOfYear = yday(firstDate))

# Print min and max dates for each year
temp_df %>%
  group_by(YY) %>%
  summarise(min = min(dayOfYear),
            max = max(dayOfYear),
            minMMDD = min(firstDate),
            maxMMDD = max(firstDate))

# overlapping range in day of year: 122 (2020) to 186 (2019)
temp_df = temp_df %>%
  filter(dayOfYear >= 122 & dayOfYear <= 186)

# number of observations per year for overlapping dates (n = 460 total sites)
temp_df %>%
  group_by(YY) %>%
  summarise(n = n())

#Anthro
kruskal.test(Anthropophony ~ YY, data = temp_df)
# p-value = 1.19e-7 therefore treatment groups are significantly different
(tempDunn = temp_df %>%
    dunn_test(Anthropophony ~ YY, p.adjust.method = "bonferroni"))
# 2017-2019 and 2019-2020 are significant

#Bio
kruskal.test(Biophony ~ YY, data = temp_df)
# p-value = 1.69e-6
(tempDunn = temp_df %>%
  dunn_test(Biophony ~ YY, p.adjust.method = "bonferroni"))
# 2017-2019 and 2017-2020 are significant

#Quiet
kruskal.test(Quiet ~ YY, data = temp_df)
# p-value = 2.51e-16 therefore treatment groups are significantly different
(tempDunn = temp_df %>%
    dunn_test(Quiet ~ YY, p.adjust.method = "bonferroni"))
# 2017-2018 are non-significant

temp_df %>%
  select(YY, Anthropophony, Biophony, Quiet) %>%
  group_by(YY) %>%
  summarise(Anthro = mean(Anthropophony),
            Bio = mean(Biophony),
            Quiet = mean(Quiet))

######## ROAD DIST/GROUP ANAYSIS #######
# Results: 3.2.4: Distance to roads 
temp_df = joined_df %>%
  mutate(Anthropophony = Anthropophony * 100,
         Biophony = Biophony * 100,
         Geophony = Geophony * 100,
         Quiet = Quiet * 100)

# KW and Dunn tests
# Anthro
kruskal.test(Anthropophony ~ RoadGroup, data = temp_df)
Dunn_anthro = temp_df %>%
  dplyr::select(Anthropophony, RoadGroup) %>%
  dunn_test(Anthropophony ~ RoadGroup, p.adjust.method = "bonferroni") 

# Bio *** KW is not significant : p = 0.08618 ***
kruskal.test(Biophony ~ RoadGroup, data = temp_df)
Dunn_bio = temp_df %>%
  dplyr::select(Biophony, RoadGroup) %>%
  dunn_test(Biophony ~ RoadGroup, p.adjust.method = "bonferroni") 

# Geo
kruskal.test(Geophony ~ RoadGroup, data = temp_df)

# Quiet
kruskal.test(Quiet ~ RoadGroup, data = temp_df)
Dunn_quiet = temp_df %>%
  dplyr::select(Quiet, RoadGroup) %>%
  dunn_test(Quiet ~ RoadGroup, p.adjust.method = "bonferroni") 


# visualize tests, not in manuscript
# Quiet
Dunn_anthro = Dunn_anthro %>% add_xy_position(x = "RoadGroup")
(ggAnthro = ggboxplot(temp_df, x = "RoadGroup", y = "Anthropophony", 
                     add = "jitter", notch = TRUE,
                     add.params = list("jitter", color = "black", alpha = 0.3),
                     legend.title = "", xlab = FALSE, ylab = "Percent Present", main = "Anthro Dunn Test Road Group") +
    stat_pvalue_manual(Dunn_anthro, hide.ns = TRUE) +
    rotate_x_text(angle = 45))

# Quiet
Dunn_quiet = Dunn_quiet %>% add_xy_position(x = "RoadGroup")
(ggQuiet = ggboxplot(temp_df, x = "RoadGroup", y = "Quiet", 
                     add = "jitter", notch = TRUE,
                     add.params = list("jitter", color = "black", alpha = 0.3),
                     legend.title = "", xlab = FALSE, ylab = "Percent Present", main = "Quiet Dunn Test Road Group") +
    stat_pvalue_manual(Dunn_quiet, hide.ns = TRUE) +
    rotate_x_text(angle = 45))

# Figure 8 
temp_df = joined_df %>%
  mutate(Anthropophony = Anthropophony * 100,
         Biophony = Biophony * 100,
         Geophony = Geophony * 100,
         Quiet = Quiet * 100) %>%
  select(site, Anthropophony, Biophony, Geophony, Quiet, RoadGroup) %>%
  gather(variable, value, -RoadGroup, -site)

(ggFig8 = ggplot(temp_df, aes(x = RoadGroup, y = value)) +
    geom_jitter(position = position_jitter(width = .2, height = 0), alpha = 0.5) +
    geom_boxplot(notch = TRUE, alpha = 0.7) +
    facet_wrap(~variable) +
    labs(title = "ABGQ distance to road",
         x = "Distance to road [m]",
         y = paste0("Percent present")) +
    ylim(min = 0, max = 85) +
    scale_y_continuous(breaks = c(0, 25, 50, 75), labels = c("0%", '25%', '50%', '75%')) +
    theme_Publication() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)))
