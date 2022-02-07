# Purpose: use aggregated ABGOQ values to perform analyses on wind patterns from Santa Rosa CIMIS station
# wind data: https://cimis.water.ca.gov/WSNReportCriteria.aspx
# TODO:

ll = '/projects/tropics/users/cquinn/R_400/'
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix, lib.loc = ll) # Kruskal Wallis
library(readxl, lib.loc = ll)

# location for agg data and results
wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/' #//pop.hpc.nau.edu
#by_hour_df = read.csv(paste0(wd,"inference_aggregation_nonXC/averages/site_avg_ABGOQU_fscore_075-ABG_ROIs_removed_daytime_5am-8pm-withError_sites_removed.csv"))
by_hour_df = read.csv(paste0(wd,"inference_aggregation_nonXC/averages/site_by_hour_ABGOQU_fscore_075-ABG_ROIs_removed-withError_sites_removed.csv"))
# update names to be more friendly for plotting
oldnames = c("Anthro_bin_mean", "Bio_bin_mean", "Geo_bin_mean", "Other_bin_mean", "Quiet_bin_mean", "forced_q_mean")
newnames = c("Anthrophony", "Biophony", "Geophony",  "Interference", "Quiet", "Unidentified")
by_hour_df = by_hour_df %>%
  drop_na() %>%
  rename_at(vars(oldnames), ~ newnames) %>%
  dplyr::select(Anthrophony,Biophony,Geophony,Interference,Quiet,Unidentified,site,HH)

by_hour_df$YY = as.integer(substr(by_hour_df$site, 10, 11))
by_hour_df$MM = as.integer(substr(by_hour_df$site, 12, 13))
by_hour_df$DD = as.integer(substr(by_hour_df$site, 14, 15))
by_hour_df$date = paste0('20', by_hour_df$YY, "-", by_hour_df$MM, "-", by_hour_df$DD)
by_hour_df$DOY = lubridate::yday(by_hour_df$date) # day
by_hour_df$Date = as.Date(paste0(by_hour_df$YY[1],'/', by_hour_df$MM, '/', by_hour_df$DD), format = '%y/%m/%d')
by_hour_df$Date_HH = by_hour_df$Date + hours(by_hour_df$HH)

# basic GIS data
site_locs = read.csv("/scratch/cq73/projects/S2L/abg_cnn/move_to_projects/s2l_location_data_airtable_201201.csv")

# distance data
#road_dist = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_streets_meters_joined.csv'))
road_devo = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_DEVELOPED_roads_meters_joined_final.csv'))
#road_undevo = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_UNDEVELPOED_streets_meters_joined.csv'))
stream_dist = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_streams_meters_joined.csv'))

# LCLU 
lclu = read_excel(paste0(wd, 'environmental_ABG_model/GIS_products/site_landcover_area_50mbuff.xlsx'))

# Wind data https://cimis.water.ca.gov/WSNReportCriteria.aspx
wind_dir = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/data/GIS_analyses/SantaRosa_CIMIS_weather_data/'
wind_files = list.files(wind_dir, pattern = "*.csv$")
wind_list = list()
for(i in seq_along(wind_files)) wind_list[[i]] = read.csv(paste0(wind_dir, wind_files[i]))
wind_df = do.call("rbind", wind_list) %>%
  dplyr::select(Stn.Name, Date, Hour..PST., Wind.Speed..mph.,qc.7, Precip..in.)

# check qc flags and remove problematic ones
wind_df %>%
  dplyr::select(qc.7) %>%
  group_by(qc.7) %>%
  count()
# I (incomplete/ignore) == 1408 entries
# M (missing) == 3
# complete == 13277
wind_df = wind_df %>%
  drop_na() %>% # 4 NA entries
  filter(!(qc.7 =='I' | qc.7 =='M')) # 1411 I or M entries
wind_df$Date = as.Date(wind_df$Date, format = '%m/%d/%Y')
wind_df$Date_HH = wind_df$Date + hours(wind_df$Hour..PST./100)

# look at hourly wind and rain
weather_df = do.call("rbind", wind_list) %>%
  dplyr::select(Stn.Name, Date, Hour..PST., Wind.Speed..mph.,qc.7,Precip..in.,qc.1)
weather_df %>%
  dplyr::select(qc.1) %>%
  group_by(qc.1) %>%
  count()
weather_df = weather_df %>%
  drop_na() %>% # 4 NA entries
  filter(!(qc.7 =='I' | qc.7 =='M' | qc.1 == 'M' | qc.1 == 'R' | qc.1 == 'Y')) # 1411 I or M entries

# daily hour pattern
wx = weather_df %>%
  dplyr::select(Hour..PST., Wind.Speed..mph., Precip..in.) %>%
  dplyr::group_by(Hour..PST.) %>%
  dplyr::summarise(u_wind = mean(Wind.Speed..mph.),
            u_ppt = mean(Precip..in.)) %>%
  gather(variable, value, -Hour..PST.)

ggplot(wx, aes(x = Hour..PST., y = value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, scales="free")
  
wx %>%
  filter(variable == 'u_wind') %>%
  ggplot(aes(x = Hour..PST., y = value)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Time of Day", 
       y = 'Wind Speed [mph]', 
       title = 'Average hourly wind speed 2017-2020, March-July') +
  scale_x_continuous(breaks = c(0000, 0500, 1000, 1500, 2000), labels = c("12AM", '5AM', '10AM', '3PM', '8PM'))

# monthly annual amount of rain
temp = weather_df %>%
  mutate(DATE = as.Date(weather_df$Date, "%m/%d/%Y"),
         DD = lubridate::day(DATE),
         MM = lubridate::month(DATE),
         YY = lubridate::year(DATE)) %>%
  group_by(YY,MM,DD) %>%
  dplyr::summarise(dailyPPT = sum(Precip..in.)) %>%
  group_by(MM, DD) %>%
  dplyr::summarise(dailyAnnualPPT = mean(dailyPPT))

temp$Date = as.Date(paste0(temp$DD,'/',temp$MM), "%d/%m")

ggplot(data = temp, aes(x = Date, y = dailyAnnualPPT)) +
  geom_smooth()

temp %>%
  group_by(MM) %>%
  dplyr::summarise(sum(dailyAnnualPPT))
  
  
  mutate(MM = as.integer(substr(Date, 0,1)),
         DD = as.integer(substr(Date, 3,4)))
  dplyr::select(MM,Wind.Speed..mph.,Precip..in.) %>%
  group_by(MM) %>%
  dplyr::summarise(sum(Precip..in.))
  
# summarize annual date ranges
by_hour_df %>%
  dplyr::select(YY,MM, DD) %>%
  group_by(YY) %>%
  summarise(min_M = min(MM),
            max_M = max(MM))
# 2017: 4 - 12
# 2018: 1 - 7
# 2019: 3 - 7
# 2020: 3 - 7
# looks like slice from March (3) thru July (7)
by_hour_df = by_hour_df %>%
  filter(MM == 3 | MM == 4 | MM == 5 | MM == 6 | MM == 7)

# function to pull desired stats : will create new col with original Col_name + "mean" or "var"
mean_var <- list(
  mean = ~mean(.x, na.rm = TRUE),
  var = ~var(.x, na.rm = TRUE),
  sd = ~sd(.x, na.rm = TRUE)
)
########################################
######## JOIN DATAFRAMES ###############
joined_df = merge(x = by_hour_df, y = lclu, by.x = 'site', by.y = 'SiteID') # removes 11 sites

# clean duplicate points in road dist
xy_df = road_devo
sites = by_hour_df$site
xy_sites = xy_df[xy_df$SiteID %in% sites,] # 821 records
xy_duplicated = xy_sites[duplicated(xy_sites$SiteID),] # happen to be the correct duplicates (61 sites)
duplicate_sites = as.character(unique(xy_duplicated$SiteID)) # sites that were duplicates
non_duplicate_sites = as.character(sites[!sites %in% duplicate_sites]) # sites without duplicates (710 sites)
xy_sites = xy_sites[xy_sites$SiteID %in% non_duplicate_sites,]
xy_sites = rbind(xy_sites, xy_duplicated) # 760 sites
road_dist = xy_sites

# clean duplicate points in stream dist
xy_df = stream_dist
sites = by_hour_df$site
xy_sites = xy_df[xy_df$SiteID %in% sites,] # 821 records
xy_duplicated = xy_sites[duplicated(xy_sites$SiteID),] # happen to be the correct duplicates (61 sites)
duplicate_sites = as.character(unique(xy_duplicated$SiteID)) # sites that were duplicates
non_duplicate_sites = as.character(sites[!sites %in% duplicate_sites]) # sites without duplicates (710 sites)
xy_sites = xy_sites[xy_sites$SiteID %in% non_duplicate_sites,]
xy_sites = rbind(xy_sites, xy_duplicated) # 760 sites
stream_dist = xy_sites

# add road dist
joined_df = merge(x = joined_df, y = road_dist, by.x = 'site', by.y = 'SiteID')
joined_df = merge(x = joined_df, y = stream_dist, by.x = 'site', by.y = 'SiteID')

# select useful vars
mod_df = data.frame('site' = joined_df$site, 
                    'Anthrophony' = joined_df$Anthrophony, 
                    'Biophony' = joined_df$Biophony, 
                    'Quiet' = joined_df$Quiet,
                    'Geophony' = joined_df$Geophony,
                    'Unidentified' = joined_df$Unidentified,
                    'Interference' = joined_df$Interference,
                    'Date' = joined_df$Date,
                    'Date_HH' = joined_df$Date_HH,
                    'HH' = joined_df$HH,
                    'DD' = joined_df$DD,
                    'MM' = joined_df$MM,
                    'YY' = joined_df$YY,
                    'LCLU' = as.factor(joined_df$MaxClass),
                    'RoadDist' = joined_df$NEAR_DIST.x,
                    'StreamDist' = joined_df$NEAR_DIST.y)

# rename LCLU
# structural order: UD, AB, HB, SH, RW, FO, FC
mod_df$LCLU_full[mod_df$LCLU == "UD"] <- "Urban/Developed"
mod_df$LCLU_full[mod_df$LCLU == "AB"] <- "Agriculture/Barren"
mod_df$LCLU_full[mod_df$LCLU == "HB"] <- "Herbaceous"
mod_df$LCLU_full[mod_df$LCLU == "SH"] <- "Shrub"
mod_df$LCLU_full[mod_df$LCLU == "RW"] <- "Riparian/Wetland"
mod_df$LCLU_full[mod_df$LCLU == "FO"] <- "Oak Forest"
mod_df$LCLU_full[mod_df$LCLU == "FC"] <- "Conifer Forest"
mod_df$LCLU_full = factor(mod_df$LCLU_full, levels = c("Urban/Developed",
                                                      "Agriculture/Barren",
                                                      "Herbaceous",
                                                      "Shrub",
                                                      "Riparian/Wetland",
                                                      "Oak Forest",
                                                      "Conifer Forest"))

# Join mod_df and wind_speed df based on YY-MM-DD-HH
mod_df = merge(x = mod_df, y = wind_df, by = 'Date_HH')


########################################
######## BASIC VIZ #####################
melted_df = mod_df %>%
  dplyr::select(-c('LCLU','Stn.Name','Hour..PST.','qc.7','Date.y','Date.x','RoadDist','StreamDist'))

# all hourly observations of Wind
temp_df = melted_df %>%
  dplyr::select(Wind.Speed..mph., Geophony, Interference) %>%
  reshape2::melt(id.vars = "Wind.Speed..mph.")

ggplot(data = temp_df, aes(y = value, x = Wind.Speed..mph.)) +
  geom_point(alpha = 0.3) +
  facet_grid(~variable) +
  labs(x = "Wind Speed [mph]", 
       y = '% time present', 
       title = 'Predicted percent present relative to hourly Wind Speed')


# Average wind speed hourly
temp_df = melted_df %>%
  dplyr::select(Wind.Speed..mph., Geophony, Interference, HH) %>%
  group_by(HH) %>%
  summarise_all('mean') %>%
  reshape2::melt(id.vars = c('HH','Wind.Speed..mph.'))

ggplot(data = temp_df, aes(y = value, x = Wind.Speed..mph.)) +
  geom_point() +
  geom_smooth() +
  facet_grid(~variable) +
  labs(x = "Wind Speed [mph]", 
       y = '% time present', 
       title = 'Average Predicted percent present relative to hourly Wind Speed')

# stratify by LCLU
temp_df = melted_df %>%
  dplyr::select(Wind.Speed..mph., Geophony, Interference, HH,LCLU_full) %>%
  group_by(HH, LCLU_full) %>%
  summarise_all('mean') %>%
  reshape2::melt(id.vars = c('HH','LCLU_full','Wind.Speed..mph.'))

ggplot(data = temp_df, aes(y = value, x = Wind.Speed..mph., colour = variable)) +
  geom_point() +
  #scale_colour_gradient2() +
  geom_smooth() +
  facet_wrap(~LCLU_full) +
  guides(colour=guide_legend(title = '')) +
  labs(x = "Wind Speed [mph]", 
       y = '% time present', 
       title = 'Average Predicted percent present relative to hourly Wind Speed')


# All LCLU facets
temp_df = melted_df %>%
  dplyr::select(Wind.Speed..mph., Geophony, Interference, HH, Anthrophony, Biophony, Quiet, LCLU_full) %>%
  group_by(HH, LCLU_full) %>%
  summarise_all('mean') %>%
  reshape2::melt(id.vars = c('HH','LCLU_full','Wind.Speed..mph.')) %>%
  mutate(variable = factor(variable, levels = c('Anthrophony','Biophony','Quiet','Geophony','Interference')))

(ggLCLUWind = ggplot(temp_df, aes(y = value * 100, x = Wind.Speed..mph., colour = variable)) +
  geom_point() +
  #scale_colour_gradient2() +
  geom_smooth() +
  facet_wrap(~LCLU_full) +
  scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c("0%", '20%', '40%', '60%')) +
  scale_colour_Publication() +
  theme_Publication() +
  guides(colour=guide_legend(title = '')) +
  labs(x = "Wind Speed [mph]", 
       y = 'Percent present', 
       title = 'Average Predicted percent present relative to hourly Wind Speed'))

ggsave(plot = ggLCLUWind,  
       filename = paste0('/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/ABG_analyses/line_plots/', "fscore075_LCLU_avgWindSpeed_ABGOQ.png"), 
       width = 12, height = 8, units = "in")



# Annual patterns
melted_df %>%
  dplyr::select(MM,Wind.Speed..mph.,Precip..in.) %>%
  group_by(MM) %>%
  dplyr::summarise(sum(Precip..in.))
