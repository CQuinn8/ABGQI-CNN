# Purpose : (1) Get unique S2L site names and (2) aggregate CNN inference
# important : this script currently (10March21) only aggregates non-overlapping mfcc preds 

library(data.table)
library(dplyr)

# inference csvs from CNN : one csv = 1 wav
inference_dir = '//pop.hpc.nau.edu/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/cnn_inference/'
inference_csvs = list.files(inference_dir, pattern = "*.csv", full.names = FALSE)
results_dir = '//pop.hpc.nau.edu/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/inference_aggregation/'


# retrieve all unique site names
rec_name = lapply(inference_csvs, function(x) strsplit(x, "_")[[1]][1]) # get recorder tag
date = lapply(inference_csvs, function(x) strsplit(x, "_")[[1]][2]) # get YYMMDD initial recording
date = lapply(date, function(x) strsplit(x, "-")[[1]][1]) # one site (s2lam057_190619-) is mis-labeled with "-" instead of "_" 
site_names = unique(paste0(rec_name,"_",date))

# save list of site names
# write.csv(as.data.frame(site_names), row.names = FALSE,
#           paste0(results_dir, "acoustic_site_names.csv"))


# create thresholded class values for ABGOQ*
labels = c("Anthro","Bio","Geo","Other","Quiet")
label_threshes = list()
for(target_label in labels){
  print(target_label)
  # read in optimal threshold based on model optimization
  th_df = read.csv(paste0('//nau.froot.nau.edu/cirrus/scratch/cq73/projects/S2L/abg_cnn/results/CNN_inference/performance/', 
                          target_label, 
                          "_sigmoid_model_acc_metrics.csv"))
  th = subset(x = th_df, X == "th")$test
  
  label_threshes[target_label] = th
}


# iterate through every site's wavs (using only non-overlapping mfcc preds (10March21))
for(s in 1:length(site_names)){
  temp_site = site_names[s]
  temp_out_path =  paste0(results_dir,"non_aggregated_by_site/",temp_site,".csv")
  
  # check if binariaztion has been done
  if(file.exists(temp_out_path)){
    print(paste0(s," : ",temp_site, " has already been binarized... skipping"))
  } else  {
    print(paste0(s, ":", temp_site))
    
    # site specific wavs
    site_wavs = inference_csvs[inference_csvs %like% temp_site]
    
    # iterate throgh each wav to threshold probabilities
    site_preds = list()
    for(i in 1:length(site_wavs)){
      temp_csv = read.csv(paste0(inference_dir,site_wavs[i]))
      
      # select only non-overlapping preds for now (10March21)!!
      r = 1:nrow(temp_csv)
      temp_csv = temp_csv[r%%2==1,]
      
      temp_csv$wav = site_wavs[i] # store wav name 
      
      # binarize each label predictions
      for(j in 1:length(names(label_threshes))){
        temp_th = label_threshes[[j]]
        temp_label = paste0(names(label_threshes)[j],"_bin")
        temp_csv[temp_label] = ifelse(temp_csv[,(j+1)] >= temp_th, 1, 0) # calculate classes based on optimized threshold
      }
      # store new df in list  
      site_preds[[i]] = temp_csv
    }
    # convert list to df
    site_df = do.call("rbind",site_preds)
    write.csv(site_df, row.names = FALSE, temp_out_path)
  }
}


#######################################
# Site Average %
site_csvs = list.files(paste0(results_dir,"non_aggregated_by_site/"), full.names = F, pattern = "*.csv")

# function to pull desired stats : will create new col with original Col_name + "mean" or "var"
mean_var <- list(
  mean = ~mean(.x, na.rm = TRUE), 
  var = ~var(.x, na.rm = TRUE)
)

# instantiate empty list
site_avg_list = list()
for(i in 1:length(site_csvs)){
  temp_site = site_csvs[i]
  temp_site_name = strsplit(temp_site, ".csv")[[1]][1]
  print(paste0(i," : ", temp_site_name))
  
  #read in csv
  df = read.csv(paste0(results_dir,"non_aggregated_by_site/",temp_site))
  n = length(unique(df$wav)) # number of recordings
  
  # get column-wise statistics on integer columns only (e.g. binarized)
  temp_avg = df %>%
    summarise(across(where(is.integer), mean_var)) 
  
  # store site name and stats
  site_avg_list[[i]] = as.data.frame(c("site" = temp_site_name, "n_min" = n, temp_avg))
}

# concat all site avgs
all_site_avg_df = do.call("rbind", site_avg_list)

# save csv
write.csv(all_site_avg_df, file = paste0(results_dir, "site_avg_ABGOQ.csv"), row.names = FALSE)


#######################################
# by-hour average % 
site_csvs = list.files(paste0(results_dir,"non_aggregated_by_site/"), full.names = F, pattern = "*.csv")

# function to pull desired stats : will create new col with original Col_name + "mean" or "var"
mean_var <- list(
  mean = ~mean(.x, na.rm = TRUE), 
  var = ~var(.x, na.rm = TRUE)
)

# instantiate empty list
by_hour_list = list()
for(i in 1:length(site_csvs)){
  temp_site = site_csvs[i]
  temp_site_name = strsplit(temp_site, ".csv")[[1]][1]
  print(paste0(i," : ", temp_site_name))
  
  #read in csv
  df = read.csv(paste0(results_dir,"non_aggregated_by_site/",temp_site))
  
  # create HH column based on wav name
  HH_temp = strsplit(as.character(df$wav), "_") 
  HH_temp = lapply(HH_temp, function(x) x[length(x)]) # pull out final chars ('HH_mm.csv')
  temp = HH_temp
  HH_temp = as.integer(lapply(HH_temp, function(x) strsplit(x, "-")[[1]][1])) # pull out HH
  df$HH = HH_temp
  
  # force quiet to positive if no other labels are present
  temp = df %>%
    mutate(forced_q = ifelse( "Anthro_bin_mean" == 0,1,0))
  
  # get column-wise statistics on integer columns only (e.g. binarized)
  temp_avg = df %>%
      group_by(HH) %>%
    summarise(across(where(is.integer), mean_var))
  
  # count n wavs per hour
  n = df %>%
    group_by(HH) %>%
    summarise(n = n()/30)
  
  # join count and stats 
  temp_avg = merge(x = temp_avg, y = n, by = 'HH')
  
  # store site name and stats
  by_hour_list[[i]] = as.data.frame(c("site" = temp_site_name, temp_avg))
}

# concat all site avgs
all_site_by_hour_df = do.call("rbind", by_hour_list)

# save csv
write.csv(all_site_by_hour_df, file = paste0(results_dir, "site_by_hour_ABGOQ.csv"), row.names = FALSE)
