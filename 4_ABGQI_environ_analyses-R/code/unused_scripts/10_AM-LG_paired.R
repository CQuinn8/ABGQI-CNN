# Purpose: use aggregated ABGOQ values to 
# VISUALS TO DO
# - by month color across hours
# - by year color across hours

ll = '/projects/tropics/users/cquinn/R_400/'
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4, lib.loc = ll)


# Paired data 
paired_df = read.csv('/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/data/colocated_ARUs-paired.csv')


# Read in each site's full audio data
pred_dir = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/inference_aggregation_nonXC/fscore_075-ABG_ROIs_removed/non_aggregated_by_site/'
abg_list = list()
for(i in seq_along(paired_df$SiteID)){
  temp_site = paired_df$SiteID[i]
  print(paste0(i, " : ", temp_site))
  tryCatch({
    abg_list[[i]] = read.csv(paste0(pred_dir, temp_site, '.csv'))
  }, error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
    print(paste0(pred_dir, temp_site, '.csv'))
  })
  
}


####
















# location for agg data and results
wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/' #//pop.hpc.nau.edu
#by_hour_df = read.csv(file = paste0(wd, "inference_aggregation/site_by_hour_ABGOQ.csv"))[,-1]
by_site_df = read.csv(file = paste0(wd, "inference_aggregation/site_avg_ABGOQ_and_forced_sixth_class.csv"))



# create additonal useful metrics
by_site_df$rec = as.factor(substr(by_site_df$site, 4,5))
by_site_df$YY = as.integer(substr(by_site_df$site, 10, 11))
by_site_df$MM = as.integer(substr(by_site_df$site, 12, 13))
by_site_df$DD = as.integer(substr(by_site_df$site, 14, 15))
by_site_df$date = paste0('20', by_site_df$YY, "-", by_site_df$MM, "-", by_site_df$DD)
by_site_df$JulDate = julian(as.Date(by_site_df$date))

########################################
######## RECORDER ANALYSIS #############
# Visualizations
subset_data = by_site_df %>%
  select(rec, Anthro_bin_mean, Bio_bin_mean, Geo_bin_mean, Other_bin_mean) %>%
  reshape2::melt(id = 1) 

# viz distribution of data (add small value an log transform)
ggplot(data = subset_data, aes(x = value)) +
  geom_histogram(aes(fill = rec)) +
  theme_bw() + 
  scale_y_log10() +
  facet_grid(~variable)

gg = ggplot(data = subset_data, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = rec), alpha = 0.8) +
  geom_point(position = position_jitterdodge(0.2), aes(colour = rec), alpha= 0.2) +
  scale_y_log10() +
  theme_bw() +
  labs(title = "Difference among ABGO presence (log) between recorder types",
       x = "Class",
       y = "log[Percent present]")

ggsave(plot = gg,  
       filename = "/scratch/cq73/projects/S2L/abg_cnn/move_to_projects/ABGO_recorder_variation_boxplot.png", 
       width = 8, height = 8, units = "in")

# Modeling
# simple linear model
fitA = lm(Anthro_bin_mean ~ rec, data = by_site_df)

fitB = lm(Bio_bin_mean ~ rec, data = by_site_df)

fitG = lm(Geo_bin_mean ~ rec, data = by_site_df)

fitO = lm(Other_bin_mean ~ rec, data = by_site_df)

fitQ = lm(Quiet_bin_mean ~ rec, data = by_site_df)

fit_forced = lm(forced_q_mean ~ rec, data = by_site_df)
summary(fitA)
summary(fitB)
summary(fitG)
summary(fitO)
summary(fitQ) # only decent R2 
summary(fit_forced)

plot(fit)

fit = lmer(Anthro_bin_mean ~ 1 + (1 | rec), data = by_site_df)
summary(fit) 
# can see rec variance (0.0002) is lower by an order of Mag than pure var (0.008) and based on boxplots rec is probably not a mixed effect
fit = lmer(Anthro_bin_mean ~ rec + (1 | n_min), data = by_site_df)
summary(fit)

# Overall it appears recorder alone is not a good predictor for ABGOQ or forced. 
#   All classes are significant but the only notable R2 value is for Quiet (0.34), otherwise they are all <0.01

# correlations 
cor(x = by_site_df$Anthro_bin_mean, y = as.numeric(by_site_df$rec), method = "spearman")
cor(x = by_site_df$Bio_bin_mean, y = as.numeric(by_site_df$rec), method = "spearman")
cor(x = by_site_df$Geo_bin_mean, y = as.numeric(by_site_df$rec), method = "spearman")
cor(x = by_site_df$Other_bin_mean, y = as.numeric(by_site_df$rec), method = "spearman")
cor(x = by_site_df$Quiet_bin_mean, y = as.numeric(by_site_df$rec), method = "spearman")
cor(x = by_site_df$forced_q_mean, y = as.numeric(by_site_df$rec), method = "spearman")

cor(x = by_site_df$Anthro_bin_mean, y = as.numeric(by_site_df$rec), method = "pearson")
cor(x = by_site_df$Bio_bin_mean, y = as.numeric(by_site_df$rec), method = "pearson")
cor(x = by_site_df$Geo_bin_mean, y = as.numeric(by_site_df$rec), method = "pearson")
cor(x = by_site_df$Other_bin_mean, y = as.numeric(by_site_df$rec), method = "pearson")
cor(x = by_site_df$Quiet_bin_mean, y = as.numeric(by_site_df$rec), method = "pearson")
cor(x = by_site_df$forced_q_mean, y = as.numeric(by_site_df$rec), method = "pearson")



########################################
######## TEMPORAL ANALYSIS #############
df = read.csv(file = paste0(wd, "inference_aggregation/site_by_hour_ABGOQ.csv"))[,-1]

# create additonal useful metrics
df$rec = as.factor(substr(df$site, 4,5))
df$YY = as.integer(substr(df$site, 10, 11))
df$MM = as.integer(substr(df$site, 12, 13))
df$DD = as.integer(substr(df$site, 14, 15))
df$date = paste0('20', df$YY, "-", df$MM, "-", df$DD)
df$JulDate = julian(as.Date(df$date))

# Visualizations
subset_data = df %>%
  select(HH, DD, MM, YY, Anthro_bin_mean, Bio_bin_mean, Geo_bin_mean, Other_bin_mean) %>%
  reshape2::melt(id = 1:4) 

ggplot(data = subset_data, aes(x = variable, y = value)) +
  geom_boxplot(alpha = 0.8, aes(colour = as.factor(YY))) +
  geom_point(position = position_jitterdodge(0.2), alpha= 0.1, aes(colour = as.factor(YY))) +
  #scale_y_log10() +
  theme_bw() +
  #facet_grid(~variable) +
  labs(title = "Difference among ABGO presence (log) between recorder types",
       x = "Class",
       y = "log[Percent present]")
ggplot(data = subset_data, aes(x = HH, y = value)) +
  geom_violin(alpha = 0.8, aes(group = HH, fill = HH)) +
  #scale_y_log10() +
  theme_bw() +
  facet_grid(~variable) +
  labs(title = "Difference among ABGO presence (log) between recorder types",
       x = "Class",
       y = "log[Percent present]")




fit = lm(Anthro_bin_mean ~ YY + MM + DD + HH + JulDate, data = df)
summary(fit)

summary(lmer(Anthro_bin_mean ~ MM + DD + HH + (1 | YY), data = df))
