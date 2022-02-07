# Purpose: use aggregated ABGOQ values to perform analyses on wind patterns from Pepperwood MET stations
# author: "Colin Quinn <cq73@nau.edu>"
# output: 

# set working directory to home directory of repo
setwd('//shares.hpc.nau.edu/cirrus/projects/tropics/users/cquinn/s2l/code/paper_0-ABG_classification/published_repo/')

library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix) # Kruskal Wallis
library(readxl)
library(corrplot)
library(lubridate)
source('color_theme_fxs.R')
source('functions.R')

# location for agg data and results
data_dir = './4_ABGQI_environ_analyses-R/data/'
df = read.csv(file = paste0(data_dir, "site_by_hour_ABGQIU_fscore_075.csv"))

# create timestamp related data
df$YY = as.integer(substr(df$site, 10, 11))
df$MM = as.integer(substr(df$site, 12, 13))
df$DD = as.integer(substr(df$site, 14, 15))
df$date = paste0('20', df$YY, "-", df$MM, "-", df$DD)
df$DOY = yday(df$date) # day
df$Date = as.Date(paste0(df$YY[1],'/', df$MM, '/', df$DD), format = '%y/%m/%d')
df$Date_HH = df$Date + hours(df$HH)

# basic GIS data
site_locs = read_excel(paste0(data_dir,"pepperwood_s2l_intersect_18-20_nearest_MET.xls"))
site_locs = site_locs %>%
  dplyr::select(FID, PointID, SiteID, Latitude, Longitude, UTM_Eastin, UTM_Northi, NEAR_FID, NEAR_DIST)
site_locs$MET_name = recode(site_locs$NEAR_FID, '0' = 'Grass', '1' = 'Redwood', '2' = 'Rogers', '3' = 'Upper Martin', '4' = 'Lower Martin')
sites_Pepperwood = site_locs$SiteID 

# distance data
#road_dist = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_streets_meters_joined.csv'))
road_devo = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_DEVELOPED_roads_meters_joined_final.csv'))
#road_undevo = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_UNDEVELPOED_streets_meters_joined.csv'))
stream_dist = read.csv(paste0(wd, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_streams_meters_joined.csv'))

# LCLU 
lclu = read_excel(paste0(wd, 'environmental_ABG_model/GIS_products/site_landcover_area_50mbuff.xlsx'))

# Wind data 
wind_dir = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/data/GIS_analyses/Pepperwood_wx/PepperwoodMetData_S2L_Quinn,C/'
wind_files = list.files(wind_dir, pattern = "*.csv$")
met = read.csv(paste0(wind_dir, 'metadata/VariableMetaData_New_MET.csv'))

#TODO Manually read in each wx csv and select the relevant vars to include
# also look at the metadata explanations

tempGrass = read.csv(paste0(wind_dir, wind_files[1])) %>%
  dplyr::select(timeStamp, RN, windSpeed, airTemp, rainHun15min, RH, flag, flagCats)
tempGrass$site = 'Grass'

sites = c('Grass','Lower_Martin', 'Redwood', 'Rogers', 'Upper_Martin')
wind_list = list()
for(i in 2:length(wind_files)){
  temp = read.csv(paste0(wind_dir, wind_files[i])) %>%
    dplyr::select(TIMESTAMP, RECORD, WindSpd_WVc1, AirTemp_F_Avg, RainGauge_Tot, RH_Avg, flag, flagCats) %>%
    dplyr::rename(timeStamp = TIMESTAMP, RN = RECORD, windSpeed = WindSpd_WVc1, airTemp = AirTemp_F_Avg, rainHun15min = RainGauge_Tot, RH = RH_Avg)
  temp$site = sites[i]
  wind_list[[i]] = temp
}
wind_df = do.call("rbind", wind_list)
wind_df = rbind(wind_df, tempGrass)
wind_df$timeStamp = gsub('T',' ',wind_df$timeStamp)
wind_df$timeStamp = gsub('Z', '', wind_df$timeStamp)
wind_df$Date = as.Date(wind_df$timeStamp)
wind_df$HHMMSS = format(as.POSIXct(wind_df$timeStamp), format = "%H:%M:%S")
wind_df$Date_HH = as.POSIXct(wind_df$timeStamp, tz = 'UTC')
wind_df$Date_HH = as.POSIXct(trunc(wind_df$Date_HH, units = "hour"))
   

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
# by_hour_df = by_hour_df %>%
#   filter(MM == 3 | MM == 4 | MM == 5 | MM == 6 | MM == 7)

# function to pull desired stats : will create new col with original Col_name + "mean" or "var"
mean_var <- list(
  mean = ~mean(.x, na.rm = TRUE),
  var = ~var(.x, na.rm = TRUE),
  sd = ~sd(.x, na.rm = TRUE)
)

########################################
######## JOIN DATAFRAMES ###############
# Select only Pepperwood sites
by_hour_df = by_hour_df[by_hour_df$site %in% sites_Pepperwood,] #133 pepperwood sites

joined_df = merge(x = by_hour_df, y = lclu, by.x = 'site', by.y = 'SiteID') # removes 11 sites
joined_df = merge(x = joined_df, y = site_locs, by.x = 'site', by.y = 'SiteID')

# select useful vars
mod_df = data.frame('site' = joined_df$site, 
                    'n_recs' = joined_df$n,
                    'Anthrophony' = joined_df$Anthrophony, 
                    'Biophony' = joined_df$Biophony, 
                    'Quiet' = joined_df$Quiet,
                    'Geophony' = joined_df$Geophony,
                    'Unidentified' = joined_df$Unidentified,
                    'Interference' = joined_df$Interference,
                    'Date' = joined_df$Date,
                    'Date_HH' = as.POSIXct(joined_df$Date_HH, tz = 'UTC'),
                    'HH' = joined_df$HH,
                    'YY' = joined_df$YY,
                    'LCLU' = as.factor(joined_df$MaxClass),
                    'MET_name' = joined_df$MET_name,
                    'Recorder' = as.factor(substr(joined_df$site, 4,5)))

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
mod_df = mod_df[!duplicated(mod_df[,1:13]), ]


# Join mod_df and wind_speed df based on YY-MM-DD-HH
wind_df$MET_name = wind_df$site
mod_df = merge(x = mod_df, y = wind_df, by = c('MET_name','Date_HH'))

temp_df = mod_df %>%
  dplyr::select (-c(RN, timeStamp, flag, flagCats, site.y, Date.y, HHMMSS)) %>% # drop vars not in analyses
  group_by(site.x, Date.x, HH) %>% # want site hourly pattern 
  dplyr::mutate(windSpeed = mean(windSpeed),
         airTemp = mean(airTemp),
         rain = mean(rainHun15min)) %>%
  distinct(Date_HH, site.x, .keep_all = TRUE)
length(unique(mod_df$site.x))

########################################
######## BASIC VIZ #####################
melted_df = mod_df %>%
  dplyr::select(-c(Date_HH, Date.x,LCLU,site.x))
melted_df = melted_df[,-c(1,2)]

# all hourly observations of Wind ~ Geo + Int
temp_df = melted_df %>%
  dplyr::select(windSpeed, Geophony, Interference, Biophony, Unidentified, Quiet, Anthrophony) %>%
  reshape2::melt(id.vars = "windSpeed")

ggplot(data = temp_df, aes(y = value, x = windSpeed)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~variable) +
  geom_smooth() +
  labs(x = "Wind Speed [mph]", 
       y = '% time present', 
       title = 'Predicted percent present relative to hourly Wind Speed')

# Average wind speed hourly~ Geo + Int
temp_df = melted_df %>%
  dplyr::select(windSpeed, Geophony, Interference, HH, Biophony, Unidentified, Quiet, Anthrophony) %>%
  group_by(HH) %>%
  summarise_all('mean') %>%
  reshape2::melt(id.vars = c('HH','windSpeed'))

ggplot(data = temp_df, aes(y = value, x = windSpeed)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable) +
  labs(x = "Wind Speed [mph]", 
       y = '% time present', 
       title = 'Average Predicted percent present relative to hourly Wind Speed')
# HH on x axis
temp_df = melted_df %>%
  dplyr::select(windSpeed, HH, Interference, Geophony, Biophony, Unidentified, Quiet, Anthrophony) %>%
  group_by(HH) %>%
  summarise_all('mean') %>%
  reshape2::melt(id.vars = c('HH', 'windSpeed'))

# ggplot(data = temp_df, aes(y = windSpeed, x = value, colour = value, size = value)) +
ggplot(data = temp_df, aes(y = value, x = windSpeed)) +
  geom_point(alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free") +
  labs(title = 'Average Predicted percent present relative to hourly Wind Speed')

# stratify by LCLU ~ Geo + Int
temp_df = melted_df %>%
  dplyr::select(windSpeed, Geophony, Interference, HH,LCLU_full) %>%
  group_by(HH, LCLU_full) %>%
  summarise_all('mean') %>%
  reshape2::melt(id.vars = c('HH','LCLU_full','windSpeed'))

ggplot(data = temp_df, aes(y = value, x = windSpeed, colour = variable)) +
  geom_point() +
  #scale_colour_gradient2() +
  geom_smooth() +
  facet_wrap(~LCLU_full) +
  guides(colour=guide_legend(title = '')) +
  labs(x = "Wind Speed [mph]", 
       y = '% time present', 
       title = 'Average Predicted percent present relative to hourly Wind Speed')


# all observations of Wind/RH/Temp ~ Geo + Int
temp_df = melted_df %>%
  dplyr::select(windSpeed, airTemp, RH, Biophony) %>%
  gather(variable, value, -Biophony)

ggplot(data = temp_df, aes(y = Biophony, x = value)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~variable, scales = "free") +
  labs(y = '% time present', 
       title = '')

# insignificant
lm1 = lm(Biophony ~ airTemp, data = melted_df)
summary(lm1)

# significant RH p = 1.09e-14
lm1 = lm(Biophony ~ RH, data = melted_df)
summary(lm1)

# significant 1e-4
lm1 = lm(Biophony ~ windSpeed, data = melted_df)
summary(lm1)

# Average wx speed hourly ~ Bio
temp_df = melted_df %>%
  dplyr::select(windSpeed, airTemp, rain, RH, HH, Biophony) %>%
  group_by(HH) %>%
  summarise_all('mean') %>%
  reshape2::melt(id.vars = c('HH','Biophony'))

ggplot(data = temp_df, aes(y = value, x = HH, colour = Biophony, size = Biophony,)) +
  geom_point(alpha = 0.7) +
  #geom_smooth() +
  facet_wrap(~variable, scales = "free") +
  labs(title = 'Average Predicted percent present relative to hourly Wind Speed')

# correlations
temp_df = melted_df %>%
  dplyr::select(windSpeed, Anthrophony, Biophony, Geophony, Interference, Unidentified, Quiet)
M = cor(temp_df, method = 'spearman')
corrplot.mixed(M) 
cor.test(temp_df$windSpeed, temp_df$Interference)
cor.test(temp_df$windSpeed, temp_df$Geophony)
cor.test(temp_df$windSpeed, temp_df$Unidentified)

summary(lm(toLogit(Anthrophony+0.0000001) ~ windSpeed, data = temp_df)) # windspeed sig (pos)
summary(lm(toLogit(Biophony+0.0000001) ~ windSpeed, data = temp_df)) # wind significant (pos)
summary(lm(toLogit(Geophony+0.0000001) ~ windSpeed, data = temp_df)) # wind significant (pos)
summary(lm(toLogit(Interference+0.0000001) ~ windSpeed, data = temp_df)) # wind significant (pos)
summary(lm(toLogit(Unidentified+0.0000001) ~ windSpeed, data = temp_df)) # wind significant (pos)

summary(lm(toLogit(Quiet+0.0000001) ~ windSpeed, data = temp_df)) # wind significant (neg)


##############################################
############## LINEAR MODELS #################
temp_df = temp_df[,-11] # remove Date.x

temp = data.frame(temp_df) %>%
  dplyr::select(Anthrophony, Biophony, Quiet, Geophony, Interference, windSpeed) %>%
  gather(soundType, value, -windSpeed) %>%
  mutate(logitRate = toLogit(value)) %>%
  filter(logitRate >= -10 & logitRate <= 10) %>%
  dplyr::select(-value)

# positive
geo = temp %>% filter(soundType == 'Geophony')
geoMod = lm(logitRate ~ windSpeed, data = geo)
summary(geoMod)
plot(geoMod)

# positive
anthro = temp %>% filter(soundType == 'Anthrophony')
anthroMod = lm(logitRate ~ windSpeed, data = anthro)
summary(anthroMod)

# positive
i = temp %>% filter(soundType == 'Interference')
iMod = lm(logitRate ~ windSpeed, data = i)
summary(iMod)

# negative
q = temp %>% filter(soundType == 'Quiet')
qMod = lm(logitRate ~ windSpeed, data = q)
summary(qMod)

# not sig
bio = temp %>% filter(soundType == 'Biophony')
bioMod = lm(logitRate ~ windSpeed, data = bio)
summary(bioMod)


# temp_df = temp_df %>%
#   dplyr::select(MET_name, site.x, Anthrophony, Biophony, Quiet, Geophony, Interference,
#                 HH, YY, windSpeed, airTemp, RH, Recorder) #%>%
#   #gather(soundType, value, -site.x, -HH, -YY, -Recorder, -MET_name, -windSpeed, -airTemp, -RH)
# 
# mod_df = temp_df %>%
#   #filter(value != 0) %>% # outlier, erronious predictions
#   mutate(YY = as.factor(as.character(YY)),
#          #soundType = as.factor(soundType),
#          HH = as.factor(as.character(HH)),
#          logitRate = toLogit(value)) %>%
#   tidyr::drop_na()
# mod_df = data.frame(mod_df)
# str(mod_df)
# summary(mod_df$logitRate)
# 
# M = mod_df %>%
#   dplyr::select(logitRate, windSpeed, airTemp, RH) %>%
#   cor()
# corrplot(M)
# plot(mod_df$windSpeed, mod_df$logitRate)
# plot(mod_df$airTemp, mod_df$logitRate)
# 
# # looks like 0s and .99s are causing a multi-modal dist
# mod_df = mod_df %>%
#   filter(logitRate >= -10 & logitRate <= 10)
# 
# wind_model = lm(logitRate ~ windSpeed + soundType, data = mod_df)
# summary(wind_model)
# 
# # # Full model
# # mFull = lm(logitRate ~ HH + windSpeed + airTemp + RH + Recorder + soundType, data = mod_df)
# # summary(mFull)
# # AIC(mFull) # 25558.77
# # 
# # # simple
# # mSimple = lm(logitRate ~ soundType, data = mod_df)
# # summary(mSimple)
# # AIC(mSimple) # 25560.00
# # 
# # # STEP WISE VAR SELECTION
# # step_model = stepAIC(mFull, direction = "both", trace = FALSE)
# # summary(step_model)
# # AIC(step_model) #25525.89
# # 
# # temp_mod = lm(formula = logitRate ~ YY + RH + soundType + windSpeed, data = mod_df)
# # summary(temp_mod)
# # AIC(temp_mod)
# 
# # look if residuals are systematic, i.e. belong to any groups
# resids = residuals(wind_model)
# #hist(resids, breaks = 100)
# df = cbind(mod_df, resids)
# ggplot(data = df, aes(x = resids, fill = soundType)) +
#   geom_histogram(alpha = 0.7)
# plot(step_model)
# 
# 
# 
# # PDPS
# 
# pdp_plotter = function(model, pdp){
#   sound = unique(pdp$soundType)
#   yy = unique(pdp$YY)
#   
#   # get model predictions
#   preds = predict(model, newdata = pdp, se = T) 
#   pdp$predicted = preds$fit # y hat
#   pdp$SEpredicted = preds$se.fit # Standard error
#   pdp$uSE = pdp$predicted + pdp$SEpredicted # upper SE
#   pdp$lSE = pdp$predicted - pdp$SEpredicted # lower SE
#   
#   # convert to normal scale from logit
#   pdp$bt_predicted = fromLogit(pdp$predicted)
#   pdp$bt_uSE = fromLogit(pdp$uSE)
#   pdp$bt_lSE = fromLogit(pdp$lSE)
#   
#   gg = ggplot(data = pdp, aes(x = windSpeed, y = bt_predicted)) + 
#     geom_ribbon(aes(ymin = bt_lSE, ymax = bt_uSE), fill = 'grey', alpha = 0.6) +
#     geom_line(color = 'dark blue', size = 1.2) +
#     theme_bw() +
#     labs(title = paste0(sound,' (20', yy, ')'))
# 
#   
#   return(gg)
# }
# 
# temp_summary = getVarDesc(data = mod_df, fml = wind_model)
# pdp = getNewData(varDesc = temp_summary, pdvars = 'windSpeed', useMedian = FALSE, 
#                  factorvals = data.frame(name = c('soundType'), 
#                                          values = c('Biophony')))
# pdp_plotter(wind_model, pdp)
# 
# 
# LULCs = c("Urban/Developed","Agriculture/Barren","Herbaceous","Shrub","Riparian/Wetland","Oak Forest","Conifer Forest")
# temp_ls = list()
# for(i in seq_along(LULCs)){
#   temp_ls[[i]] = getNewData(varDesc = temp_summary, pdvars = 'logRoadDist', useMedian = FALSE, 
#                             factorvals = data.frame(name = c('YY', 'soundType', 'LULC_full'), 
#                                                     values = c('20','Anthrophony', LULCs[i])))
# }
# pdpLULC = do.call('rbind', temp_ls)
# pdp_plotter(step_model,pdpLULC)

