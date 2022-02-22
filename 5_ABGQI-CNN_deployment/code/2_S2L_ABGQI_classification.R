library(data.table)
library(dplyr)
# Local wd
wd = 'published_repo/'

error_log = paste0(wd,'5_ABGQI-CNN_deployment/results/error_logs.txt')

# where outputs will be saved
results_dir = paste0(wd,'5_ABGQI-CNN_deployment/results/classifications/')

# inference csvs from CNN : one csv = 1 wav
inference_dir = paste0(wd,'5_ABGQI-CNN_deployment/results/predictions/')
inference_csvs = list.files(inference_dir, pattern = "*.csv", full.names = FALSE)

# create thresholded class values for ABGIQ by reading in each S2L results csv
labels = c("Anthropophony","Biophony","Geophony","Interference","Quiet")
label_threshes = list()
for(target_label in labels){
  print(target_label)
  # read in optimal threshold based on model optimization
  th_df = read.csv(paste0(wd,'5_ABGQI-CNN_deployment/data/thresholds/',target_label, ".csv"))
  th = subset(x = th_df, X == "th")$test
  label_threshes[target_label] = th
}


# iterate through every wav's prediction
preds = list()
for(i in 1:length(inference_csvs)){
  temp = inference_csvs[i]
  temp_out_path =  paste0(results_dir,temp,".csv")
  
  # check if binarization has been done
  if(file.exists(temp_out_path)){
    print(paste0(i," : ",temp, " has already been binarized... skipping"))
  } else  {
    print(paste0(i, ":", temp))
    tryCatch({ 
      temp_csv = read.csv(paste0(inference_dir, temp))

      # binarize each label predictions
      for(j in 1:length(names(label_threshes))){
        temp_th = label_threshes[[j]]
        temp_label = paste0(names(label_threshes)[j],"_bin")
        temp_csv[temp_label] = ifelse(temp_csv[,(j+1)] >= temp_th, 1, 0) # calculate classes based on optimized threshold
      }
      temp_csv = temp_csv %>%
        mutate(Unidentified = ifelse(temp_csv$"Anthropophony_bin" == 0 & 
                                       temp_csv$"Biophony_bin" == 0 & 
                                       temp_csv$"Geophony_bin" == 0 & 
                                       temp_csv$"Interference_bin" == 0 &
                                       temp_csv$"Quiet_bin" == 0,
                                 1,0))
      temp_csv$wav = tools::file_path_sans_ext(temp)
      
      # save and store new df in list  
      write.csv(temp_csv, temp_out_path, row.names = FALSE)
      preds[[i]] = temp_csv
      
    }, error = function(e){
      cat("ERROR :",conditionMessage(e), "\n")
      write(toString(temp), error_log, append=TRUE)
    })
  }
}

# convert list to df and savbe all predictions in single csv (may be large if there are many recordings, e.g., >25,000)
all_pred_df = do.call("rbind", preds)
write.csv(all_pred_df, row.names = FALSE, paste0(results_dir,"all_recording_predictions.csv"))
