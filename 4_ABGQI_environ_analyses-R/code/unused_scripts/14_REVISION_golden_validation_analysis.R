# TODO
# X 1) read in ABG csv and get 1-min ABG from by-second json annotations
# X 2) verify if the remaining wavs (196) need to be listened to or not
# X 3) grab corresponding predictions
# X 4) summarize if ABG is present in ANY melspecs and how many mels (two values)
#   5) compare preds with GV (!!!)

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

#wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/golden_validation_analysis/'
# pred_dir = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/inference_aggregation_nonXC/fscore_075-ABG_ROIs_removed/non_aggregated_by_site/'
wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/AUGMENTATION/golden_validation_analysis/'
pred_dir = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/AUGMENTATION/inference_aggregation_nonXC/fscore_075-ABG_ROIs_removed/non_aggregated_by_site/'

# read in GV dataset
gv_df = read.csv(paste0(wd, 'Validation-ABG_siteID.csv'))
gv_df = gv_df[, c('Name','Recording','Seconds.of.ABG..JSON.','SiteID')]

# need to rename wavs
timestamp = sapply(gv_df$Recording, function(x) paste0(unlist(strsplit(x, '-')[[1]][-1]), collapse = '-'))
gv_df$file = tools::file_path_sans_ext(paste0(gv_df$SiteID, '_', timestamp))

###################################
# COPY GV FILES TO SEPARATE DIR ###
wav_dir1 = '/scratch/cq73/projects/S2L/distributionData/'
wav_dir2 = '/scratch/cq73/projects/S2L/audio/'
dest_dir = paste0(wd, 'GV_wavs/')

# copy files
# wrong extension
toDo = list()
cnt = 1
for(i in seq_along(gv_df$file)){
  # check first dir
  temp_wav = paste0(gv_df$file[i], '.wav')
  temp_WAV = paste0(gv_df$file[i], '.WAV')
  
  # pre 2020, wav
  if(file.exists(paste0(wav_dir1, temp_wav))){
    file.copy(paste0(wav_dir1, temp_wav), to = dest_dir)
  # post 2020, wav
  } else if(file.exists(paste0(wav_dir2, temp_wav))){
    file.copy(paste0(wav_dir2, temp_wav), to = dest_dir)
  # pre 2020, WAV  
  } else if(file.exists(paste0(wav_dir1, temp_WAV))){
    file.copy(paste0(wav_dir1, temp_WAV), to = dest_dir)
  # post 2020, WAV
  } else if(file.exists(paste0(wav_dir2, temp_WAV))){
    file.copy(paste0(wav_dir2, temp_WAV), to = dest_dir)
  } else {
    print(paste("No file for", gv_df$file[i]))
    toDo[[cnt]] = gv_df$file[i]
    cnt = cnt + 1
  }
}

###################################
# remove any incomplete recordings (here, empty text not NA)
gv_df = gv_df %>%
  dplyr::filter(Seconds.of.ABG..JSON. != "") %>%
  filter(Name != 46)

# aggregate by-second GV observations to 1-min pres/abs
anthro = gv_df[grepl('A', gv_df$Seconds.of.ABG..JSON.), , drop = FALSE]$Recording
bio = gv_df[grepl('B', gv_df$Seconds.of.ABG..JSON.), , drop = FALSE]$Recording
geo = gv_df[grepl('G', gv_df$Seconds.of.ABG..JSON.), , drop = FALSE]$Recording
quiet = gv_df[grepl('Q', gv_df$Seconds.of.ABG..JSON.), , drop = FALSE]$Recording
interference = gv_df[grepl('O', gv_df$Seconds.of.ABG..JSON.), , drop = FALSE]$Recording
unident = gv_df[grepl('US', gv_df$Seconds.of.ABG..JSON.), , drop = FALSE]$Recording

# populate as pres (1) or abs (0) for each recording
gv_df$Anthrophony = as.numeric(gv_df$Recording %in% anthro)
gv_df$Biophony = as.numeric(gv_df$Recording %in% bio)
gv_df$Geophony = as.numeric(gv_df$Recording %in% geo)
gv_df$Quiet = as.numeric(gv_df$Recording %in% quiet)
gv_df$Interference = as.numeric(gv_df$Recording %in% interference)
gv_df$Unidentified = as.numeric(gv_df$Recording %in% unident)

# grab ABGQI predictions by site
pred_list = list()
for(i in 1:(nrow(gv_df))){
  print(i)
  temp = gv_df[i,]
  temp_wav = paste0(temp$file,'.csv')
  temp_df = fread(paste0(pred_dir, temp$SiteID, '.csv'))
  
  # subset to GV wav and organize
  temp_preds = temp_df[temp_df$wav %like% temp_wav,] %>%
    select(Anthro_bin, Bio_bin, Geo_bin, Other_bin, Quiet_bin) %>%
    rename(Anthro_pred = Anthro_bin, Bio_pred = Bio_bin, Geo_pred = Geo_bin, Int_pred = Other_bin, Quiet_pred = Quiet_bin) %>%
    summarise(across(everything(), ~ sum(., is.na(.), 0)))
  temp_preds$wav = tools::file_path_sans_ext(temp_wav)
  
  # store in list Or in gv_df
  pred_list[[i]] = temp_preds
} 

pred_df = do.call(rbind, pred_list)

# join dataframes
df = merge(x = gv_df, y = pred_df, by.x = 'file', by.y = 'wav')


############################
# Viz

# mutated_df_gv = pred_df %>%
#   select(-c(Name, Recording, Seconds.of.ABG..JSON., SiteID)) %>%
#   gather(variable, value, -file)

# count of GV presences
df %>%
  select(Anthrophony, Biophony, Geophony, Quiet, Interference) %>%
  summarise(across(everything(), ~ sum(., is.na(.), 0)))

# add in prediction binary
df$Anthro_pres = ifelse(df$Anthro_pred > 0, 1, 0)
df$Bio_pres = ifelse(df$Bio_pred > 0, 1, 0)
df$Geo_pres = ifelse(df$Geo_pred > 0, 1, 0)
df$Int_pres = ifelse(df$Int_pred > 0, 1, 0)

# grouped by GV pres abs
ggplot(data = df, aes(y = Anthro_pred, x = factor(Anthrophony))) +
  geom_boxplot(alpha = 0.5, notch = T) +
  geom_jitter(position=position_jitter(width=.1, height=0))
ggplot(data = df, aes(y = Bio_pred, x = factor(Biophony))) +
  geom_boxplot(alpha = 0.5, notch = T) +
  geom_jitter(position=position_jitter(width=.1, height=0))
ggplot(data = df, aes(y = Geo_pred, x = factor(Geophony))) +
  geom_boxplot(alpha = 0.5, notch = T) +
  geom_jitter(position=position_jitter(width=.1, height=0))
ggplot(data = df, aes(y = Int_pred, x = factor(Interference))) +
  geom_boxplot(alpha = 0.5, notch = T) +
  geom_jitter(position=position_jitter(width=.1, height=0))

############################
# Evaluation metrics

pred_pres = df %>%
  select(Anthro_pres, Bio_pres, Geo_pres, Int_pres)
gv_pres = df %>%
  select(Anthrophony, Biophony, Geophony, Interference)

# iterate through each class to get accuracy metrics
results_list = list()
classes = c('Anthrophony','Biophony','Geophony','Interference')
for(i in 1:4){
  a = sum(pred_pres[,i] == 1 & gv_pres[,i] == 1) # True Positive
  b = sum(pred_pres[,i] == 1 & gv_pres[,i] == 0) # False Positive
  c = sum(pred_pres[,i] == 0 & gv_pres[,i] == 1) # False Negative
  d = sum(pred_pres[,i] == 0 & gv_pres[,i] == 0) # True Negative
  precision = a / (a + b) #PPV
  recall = a / (a + c) # aka sensitivity/TPR
  fscore = (1 + 1^2) * ((precision * recall) / ((1^2 * precision) + recall))
  # f050 = (1 + 0.5^2) * ((precision * recall) / ((0.5^2 * precision) + recall))
  # f075 = (1 + 0.75^2) * ((precision * recall) / ((0.75^2 * precision) + recall))
  
  results_list[[i]] = data.frame(TP = a,
                                 FP = b,
                                 FN = c,
                                 TN = d,
                                 Precision = precision,
                                 Recall = recall,
                                 f1 = fscore,
                                 Class = classes[i])
}
results = do.call(rbind, results_list)
