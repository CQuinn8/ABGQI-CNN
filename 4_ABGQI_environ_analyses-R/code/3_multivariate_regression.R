# Purpose: Create multivariate linear regression with LULC and road distance to explaine soundscape components
# Requires: linear_regression_df.csv
# Outputs: Table 3 in manuscript; Section 3.3
# Author: Colin Quinn <cq73@nau.edu>

# set working directory to home directory of repo
setwd('/published_repo/')

library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS) # stepwise variable selection
library(lmtest) # LRT

source('support_functions/color_theme_fxs.R')
source('support_functions/functions.R')

data_dir = '4_ABGQI_environ_analyses-R/data/'
df = read.csv(paste0(data_dir,'linear_regression_df.csv' ))

# order LULC
df$LULC = factor(df$LULC, levels = c("Urban/Developed", "Agriculture/Barren", "Herbaceous",
                                                   "Shrub", "Riparian/Wetland", "Oak Forest",
                                                   "Conifer Forest"))

########## Model setup ##########
mod_df = df %>%
  gather(soundType, value, -site, -LULC, -RoadDist, -YY, -Recorder, -n_min, -n_mel, -DOY)

# filter out zeros. Zeroes imply no melspecs had an observation which equates to an error at the site level with that class in recording
summary(mod_df$value)

mod_df = mod_df %>%
  filter(value != 0) %>% # zero counts
  mutate(YY = as.factor(as.character(YY)),
         soundType = as.factor(soundType),
         Recorder = as.character(Recorder),
         site = as.character(site),
         logRoadDist = log(RoadDist),
         logn_mel = log(n_mel),
         rate = value / n_mel,
         logitRate = toLogit(rate))

summary(mod_df$logitRate)

########## modeling ##########
# start with a simple model and full model and look at AICs
# LULC = Urban, Year = 2017, and soundtype = Anthropophony will be the reference
# Full model
mFull = lm(logitRate ~ LULC + I(logRoadDist^2) + I(logRoadDist^3) + 
             YY + logn_mel + soundType + Recorder + 
             LULC*logRoadDist + soundType:logRoadDist + DOY, 
           data = mod_df)
summary(mFull)
AIC(mFull) # 13702.69

# simple
mSimple = lm(logitRate ~ 1, data = mod_df)
summary(mSimple)
AIC(mSimple) # 14740.29

# STEP WISE VAR SELECTION
step_model = stepAIC(mFull, direction = "both", trace = TRUE)
summary(step_model)
AIC(step_model) #13687.75

# look if residuals are systematic, i.e. belong to any groups
resids = residuals(step_model)
resid_df = cbind(mod_df, resids)
ggplot(data = resid_df, aes(x = resids, fill = soundType)) +
  geom_histogram(alpha = 0.7)
ggplot(data = resid_df, aes(x = RoadDist, y = resids, colour = Recorder)) +
  geom_point()
plot(step_model)



# Test LULC removal as it is only significant in interaction with road distance
# remove LULC
step_model_noLULC = update(step_model, .~. -LULC)
summary(step_model_noLULC)
AIC(step_model_noLULC) # 13703.36

# supports keeping LULC in the model
anova(step_model_noLULC, step_model)
lrtest(step_model_noLULC, step_model)

# Try higher dimension road Dist to step model
step_model_Road2 = lm(formula = logitRate ~ YY + logn_mel + soundType + LULC + logRoadDist + I(logRoadDist^2) + soundType:logRoadDist, data = mod_df)
summary(step_model_Road2)
AIC(step_model_Road2) # 13689.72

# LRT
lrtest(step_model, step_model_Road2) #higher order road isn't advised
