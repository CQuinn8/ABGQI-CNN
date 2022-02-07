# TODO
# X 1) Calculate ABGQI pres/abs in 2021 data
#   2) Read in paired ARU csv and grab corresponding sites from aggregated preds
#     2b) manually clip the problem pairs to corresponding length (listed lines 44 -63)
#   3) Decide on proper statistical tests
#   4) Perform paired comparison for site-wise LG vs ARU
#   5) what does this imply? (corrections? elimination? etc.?)

# look at by-hour aggregation time series trends
# calc

library(dplyr)
library(tidyr)
library(ggplot2)

wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results_avec_2021/'

# read in paired ARU csv with AM site list and LG site list
paired_df = read.csv(paste0(wd,'paired_ARU/colocated_ARUs-paired.csv')) %>%
  dplyr::select(SiteID, Number.of.Recordings, Paired_site) %>%
  filter(grepl('am', SiteID)) # 24 paired sites (48 ARUs)

# read in all pairs and create dataframes of each paired set of predictions by overlapping time
pair_list = list()
for(pair in seq_along(paired_df$SiteID)){
  temp_row = paired_df[pair,]
  print(paste0("Pair: ",pair))
  
  #read in csvs
  am = read.csv(paste0(wd, 'paired_ARU/paired_ABGQI/', temp_row$SiteID, '.csv')) %>%
    dplyr::select(Anthro, Bio, Geo, Other, Quiet, wav, mfcc) %>%
    dplyr::rename(Anthrophony = Anthro, Biophony = Bio, Geophony = Geo, Interference = Other, Quiet = Quiet, wavAM = wav) %>%
    gather(variable, value, -wavAM, -mfcc)
  lg = read.csv(paste0(wd, 'paired_ARU/paired_ABGQI/', temp_row$Paired_site, '.csv')) %>%
    dplyr::select(Anthro, Bio, Geo, Other, Quiet, wav, mfcc) %>%
    dplyr::rename(Anthrophony = Anthro, Biophony = Bio, Geophony = Geo, Interference = Other, Quiet = Quiet, wavLG = wav) %>%
    gather(variable, value, -wavLG, -mfcc)

  # build dateframe
  am$DD = substr(am$wav, 25, 26)
  am$HH = substr(am$wav, 28, 29)
  am$mm = substr(am$wav, 31, 32)
  am$DDHHmm = paste0(am$DD, am$HH, am$mm)
  lg$DD = substr(lg$wav, 25, 26)
  lg$HH = substr(lg$wav, 28, 29)
  lg$mm = substr(lg$wav, 31, 32)
  lg$DDHHmm = paste0(lg$DD, lg$HH, lg$mm)
  
  pair_list[[pair]] = am %>%
    inner_join(lg, by = c('DD','HH','mm','variable','mfcc')) %>%
    dplyr::select(wavAM, variable, value.x, value.y, DDHHmm.x) %>%
    dplyr::rename(wav = wavAM, soundType = variable, DDHHmm = DDHHmm.x, AM = value.x, LG = value.y) %>%
    mutate(SiteID = pair)
  
}

pair_preds = do.call(rbind, pair_list)

# Start with visual of pairwise boxplots for each sound type (ABGQIU plots with 24 pairs of boxes)
pair_preds %>%
  gather(ARU, value, -wav, -soundType, -SiteID, -DDHHmm) %>%
  filter(soundType == 'Biophony') %>%
  ggplot(aes(x = soundType, y = value, fill = ARU, colour = ARU)) +
  geom_boxplot(notch = TRUE, alpha = 0.7, outlier.shape = NA) +
  facet_grid(~SiteID) +
  theme(axis.text.x=element_blank()) +
  labs(x ="Biophony",
       title = "ARU paired predictions")

pair_preds %>%
  gather(ARU, value, -wav, -soundType, -SiteID, -DDHHmm) %>%
  filter(soundType == 'Anthrophony') %>%
  ggplot(aes(x = soundType, y = value, fill = ARU, colour = ARU)) +
  geom_boxplot(notch = TRUE, alpha = 0.7, outlier.shape = NA) +
  facet_grid(~SiteID) +
  theme(axis.text.x=element_blank()) +
  labs(x ="Anthrophony",
       title = "ARU paired predictions")

pair_preds %>%
  gather(ARU, value, -wav, -soundType, -SiteID, -DDHHmm) %>%
  filter(soundType == 'Quiet') %>%
  ggplot(aes(x = soundType, y = value, fill = ARU, colour = ARU)) +
  geom_boxplot(notch = TRUE, alpha = 0.7, outlier.shape = NA) +
  facet_grid(~SiteID) +
  theme(axis.text.x=element_blank()) +
  labs(x ="Quiet",
       title = "ARU paired predictions")

pair_preds %>%
  #gather(ARU, value, -wav, -soundType, -SiteID, -DDHHmm) %>%
  filter(soundType == 'Biophony') %>%
  ggplot(aes(x = LG, y = AM)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~SiteID) +
  theme(axis.text.x=element_blank()) +
  labs(x ="Biophony",
       title = "ARU paired predictions")

###################################

# paired t-test
temp = pair_preds %>%
  filter(soundType == 'Anthrophony')

c = cor(temp$AM, temp$LG, method = "pearson")
# 0.299

mod = t.test(temp$AM, temp$LG, paired = TRUE)
mod
# t = 113.16, df = 319619, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.03966024 0.04105837
# sample estimates:
#   mean of the differences 
# 0.04035931 
# Anthrophony is higher ~4.0% in AMs

temp = pair_preds %>%
  filter(soundType == 'Biophony')
mod = t.test(temp$AM, temp$LG, paired = TRUE)
mod
# t = 49.298, df = 319619, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.02984585 0.03231730
# sample estimates:
#   mean of the differences 
# 0.03108157 
# Biophony is higher ~3.1% in AMs


temp = pair_preds %>%
  filter(soundType == 'Geophony')
mod = t.test(temp$AM, temp$LG, paired = TRUE)
mod
# t = 19.988, df = 319619, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01069499 0.01302041
# sample estimates:
#   mean of the differences 
# 0.0118577
# Geophony is higher ~1.2% in AMs

temp = pair_preds %>%
  filter(soundType == 'Quiet')
mod = t.test(temp$AM, temp$LG, paired = TRUE)
mod
# t = -304.54, df = 319619, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2283705 -0.2254498
# sample estimates:
#   mean of the differences 
# -0.2269102
# Quiet is lower ~23.7% in AMs

temp = pair_preds %>%
  filter(soundType == 'Interference')
mod = t.test(temp$AM, temp$LG, paired = TRUE)
mod
# t = 244.25, df = 319619, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1295907 0.1316873
# sample estimates:
#   mean of the differences 
# 0.130639 
# Interference is higher ~13.1% in AMs


###################################
# Average to the 1-min level 
by_min = pair_preds %>%
  select(-wav) %>%
  group_by(SiteID, soundType, DDHHmm) %>%
  summarise(across(everything(), list(mean)))

temp = by_min %>%
  filter(soundType == 'Quiet')
mod = t.test(temp$AM_1, temp$LG_1, paired = TRUE)
mod



###################################
# Average to the 1-hr level
pair_preds$DDHH = substr(pair_preds$DDHHmm,1,4)
by_hr = pair_preds %>%
  select(-wav) %>%
  group_by(SiteID, soundType, DDHH) %>%
  summarise(across(everything(), list(mean))) %>%
  filter(!(SiteID == 3 | SiteID == 5)) # both sites have abnormal behaviro (or lack thereof) for LGs 

by_hr %>%
  #gather(ARU, value, -wav, -soundType, -SiteID, -DDHHmm) %>%
  #filter(soundType == 'Biophony') %>%
  ggplot(aes(x = LG_1, y = AM_1, colour = soundType)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~SiteID) +
  theme(axis.text.x=element_blank()) +
  labs(x ="LG", 
       y = 'AM',
       title = "ARU paired predictions")

# Looks like these site pairs should be removed:
# 3: s2llg002_210405 (LG is silent for the whole duration)

# Bio: p = 3.6e-10; diff = 0.02
temp = by_hr %>%
  filter(soundType == 'Biophony')
mod = t.test(temp$AM_1, temp$LG_1, paired = TRUE)
mod

# Anthro: p = 2.2e-16; diff = 0.04
temp = by_hr %>%
  filter(soundType == 'Anthrophony')
mod = t.test(temp$AM_1, temp$LG_1, paired = TRUE)
mod

# Geo: p = 0.1284; diff = -0.007
temp = by_hr %>%
  filter(soundType == 'Geophony')
mod = t.test(temp$AM_1, temp$LG_1, paired = TRUE)
mod


# Quiet: p = 2.2e-16; diff = -0.19
temp = by_hr %>%
  filter(soundType == 'Quiet')
mod = t.test(temp$AM_1, temp$LG_1, paired = TRUE)
mod

# Int: p =2.2e-16; diff = 0.114
temp = by_hr %>%
  filter(soundType == 'Interference')
mod = t.test(temp$AM_1, temp$LG_1, paired = TRUE)
mod

# Correlation
c = cor(by_hr$AM_1, by_hr$LG_1, method = "pearson")
# 0.74