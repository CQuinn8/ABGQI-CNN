# author: "Colin Quinn <cq73@nau.edu>"
# output: Figure 5

# set working directory to home directory of repo
setwd('/published_repo/')

library(dplyr) # data manipulation
library(ggplot2) # plotting
library(WeightedROC) # accuracy metrics
library(e1071) # accuracy metrics
source('support_functions/color_theme_fxs.R')

# CNN inference csvs (here, a test and train/val csv for each class; n = 10 csvs)
inf_wd = '3_cnn_threshold_optimization_and_accuracy-R/data/cnn_inference/'   

# desired F-score beta threshold
betas = c('0.50', '0.75', '1.00', '1.25', '1.50') # beta = 1.5 : weights recall higher than precision vs beta = 0.5: weights recall lower than precision
labels = c("Anthropophony","Biophony","Geophony","Interference","Quiet")
csvs = list.files(inf_wd, pattern = "*.csv", full.names = F)

# combine csvs and add test/tr label
csv_ls = list()
counter = 1
for(file in csvs){
  print(file)
  label = strsplit(file, "_")[[1]][1]
  split = strsplit(file, "_")[[1]][2]
  split = strsplit(split, ".csv")[[1]]
  temp = read.csv(paste0(inf_wd,"/",file))
  colnames(temp) = c("Anthropophony*","Biophony*","Geophony*","Interference*","Quiet*")
  temp$label = label
  temp$split = split
  csv_ls[[counter]] = temp
  counter = counter + 1 
}

# dataframes for all inference, test, and combined train + validation
full_df = do.call("rbind", csv_ls)
full_test_df = subset(full_df, split == "test")
full_trval_df = subset(full_df, split == "trval")

# MODEL ANALYSIS BASED ON TESTNG DATA for each label and a range of f(beta) scores
# if you receive a font error when plotting, use this to try and debug
windowsFonts()
windowsFonts("helvetica" = windowsFont("Helvetica"))

# Iterate over each sound class (inner loop) for each beta (outer loop)
for(b in betas) {
  print(paste0("Optimizing beta = ", b))
  b_char = gsub('.','',b, fixed = TRUE)
  b_num = as.numeric(b)
  
  # where results will be saved in this loop
  out_dir = paste0("3_cnn_threshold_optimization_and_accuracy-R/results/performance_fscore_", b_char ,'/') # change beta level
  dir.create(out_dir)
  
  # sound classes
  final_thresh_ls = list()
  threshold_ls = list()
  for(i in 1:length(labels)){
    target_label = labels[i]
    print(target_label)
    
    # subset current label df
    tr_df = full_trval_df[c(paste0(target_label,"*"),"label")]
    test_df = full_test_df[c(paste0(target_label,"*"),"label")]
    
    # extract predicted probability values and one-hot encode test labels coresponding the correct column
    probabilities = test_df
    probabilities_tr = tr_df
    pred_prob_vec = probabilities[paste0(target_label,"*")] # label corresponding to prediction for current label
    pred_prob_vec_tr = probabilities_tr[paste0(target_label,"*")] 
    ts_y_ohe = ifelse(probabilities$label == tolower(target_label), 1, 0) # one-hot encode test data to current label
    tr_y_ohe = ifelse(probabilities_tr$label == tolower(target_label), 1, 0) 
    
    
    #Threshold Optimization
    thresh_dt_list = list()
    tsearch = seq(0.0001, 0.9999, by = 0.0001) # incremental steps to find best threshold value
    for (t in tsearch) {
      pred_class = ifelse(pred_prob_vec >= t, 1, 0) # pull out positive vector and binarize based on t
      
      # calculations to clean up Spec and Sens section, below
      a = sum(pred_class==1 & ts_y_ohe==1) # True Positive
      b = sum(pred_class==1 & ts_y_ohe==0) # False Positive
      c = sum(pred_class==0 & ts_y_ohe==1) # False Negative
      d = sum(pred_class==0 & ts_y_ohe==0) # True Negative
      precision = a / (a + b) #PPV
      recall = a / (a + c) # aka sensitivity/TPR
      fscore = (1 + b_num^2) * ((precision * recall) / ((b_num^2 * precision) + recall))
      f050 = (1 + 0.5^2) * ((precision * recall) / ((0.5^2 * precision) + recall))
      f075 = (1 + 0.75^2) * ((precision * recall) / ((0.75^2 * precision) + recall))
      f100 = (1 + 1^2) * ((precision * recall) / ((1^2 * precision) + recall))
      
      #assign threshold and tval to list
      thresh_dt_list[[t*10000]] <- data.table::data.table(threshold = t, f = fscore, f050 = f050, f075 = f075, f100 = f100)
    }
    thresh_dt <- do.call(rbind, thresh_dt_list)
    final_thresh <- thresh_dt[which.max(thresh_dt$f),] #select maximum fbeta value and its threshold
    pred_class_vec <- ifelse(pred_prob_vec >= final_thresh$threshold, 1, 0) # calculate classes based on optimized threshold
    pred_class_vec_tr = ifelse(pred_prob_vec_tr >= final_thresh$threshold, 1, 0)
    
    # Create confusion matrix
    a_tr = sum(pred_class_vec_tr==1 & tr_y_ohe==1) # True Positive
    b_tr = sum(pred_class_vec_tr==1 & tr_y_ohe==0) # False Positive
    c_tr = sum(pred_class_vec_tr==0 & tr_y_ohe==1) # False Negative
    d_tr = sum(pred_class_vec_tr==0 & tr_y_ohe==0) # True Negative
    precision_tr = a_tr / (a_tr + b_tr) #PPV
    recall_tr = a_tr / (a_tr + c_tr) # aka sensitivity/TPR
    fscore_tr = (1 + b_num^2) * ((precision_tr * recall_tr) / ((b_num^2 * precision_tr) + recall_tr))
    f050_tr = (1 + 0.5^2) * ((precision_tr * recall_tr) / ((0.5^2 * precision_tr) + recall_tr))
    f075_tr = (1 + 0.75^2) * ((precision_tr * recall_tr) / ((0.75^2 * precision_tr) + recall_tr))
    f100_tr = (1 + 1^2) * ((precision_tr * recall_tr) / ((1^2 * precision_tr) + recall_tr))
    confMat_ts <- caret::confusionMatrix(data = as.factor(pred_class_vec), reference = as.factor(ts_y_ohe), positive = "1")	
    confMat_tr <- caret::confusionMatrix(data = as.factor(pred_class_vec_tr), reference = as.factor(tr_y_ohe), positive = "1")
    roc_df <- WeightedROC::WeightedROC(guess = pred_prob_vec[,1], label = as.factor(ts_y_ohe))
    roc_df_tr <- WeightedROC::WeightedROC(guess = pred_prob_vec_tr[,1], label = as.factor(tr_y_ohe))
    
    out_ConfMat = data.frame("train" = c(confMat_tr$overall, 
                                         confMat_tr$byClass, 
                                         "positive" = confMat_tr$positive, 
                                         "th" = final_thresh$threshold,
                                         RMSE = sqrt(mean((pred_class_vec_tr - (tr_y_ohe))^2)),
                                         fbeta = b_num, # the beta value
                                         fscore = fscore_tr, # fscore based on current fbeats (e.g. is variable - hard to compare across)
                                         f050 = f050_tr,
                                         f075 = f075_tr,
                                         f100 = f100_tr,
                                         auc = WeightedROC::WeightedAUC(roc_df_tr)),
                             "test" = c(confMat_ts$overall, 
                                        confMat_ts$byClass, 
                                        "positive" = confMat_ts$positive, 
                                        "th" = final_thresh$threshold,
                                        RMSE = sqrt(mean((pred_class_vec - ts_y_ohe)^2)),
                                        fbeta = b_num,
                                        fscore = final_thresh$f,
                                        f050 = final_thresh$f050,
                                        f075 = final_thresh$f075,
                                        f100 = final_thresh$f100,
                                        auc = WeightedROC::WeightedAUC(roc_df)))
    
    ######### SAVE OBJECTS ##########
    # save confusion matrix object 
    write.csv(out_ConfMat, file = paste0(out_dir, target_label, "_model_acc_metrics.csv"))
    
    thresh_dt$label = target_label
    final_thresh_ls[[i]] = final_thresh
    threshold_ls[[i]] = thresh_dt
  }
  
  # turn threshold data into dataframes
  threshold_df = do.call(rbind, threshold_ls)
  threshold_df = as.data.frame(threshold_df[complete.cases(threshold_df),])
  final_thresholds = as.data.frame(do.call(rbind, final_thresh_ls))
  final_thresholds$label = c('Anthropophony','Biophony','Geophony','Interference', 'Quiet')
  final_thresholds$label = factor(final_thresholds$label, levels = c('Anthropophony','Biophony','Quiet','Geophony','Interference'))
  threshold_df$label = factor(threshold_df$label, levels = c('Anthropophony','Biophony','Quiet','Geophony','Interference'))
  
  
  # create fscore plot (Figure 5)
  ggFscore = ggplot(data = threshold_df) +
    geom_line(aes(x = threshold, y = f, colour = label), size = 1, alpha = 0.8, show.legend = F) + # Thresh lines
    geom_point(data = final_thresholds, aes(x = threshold, y = f, fill = label), size = 5, pch = 21) + # optimal thresh
    geom_text(data = final_thresholds, aes(x = threshold, y = f, label = threshold), hjust=-0.25, vjust=-1.1) + # thresh value
    theme_bw() +
    xlab("Threshold") +
    ylab(expression(paste("F(",beta,") value"))) +
    guides(fill=guide_legend(title="")) +
    ggtitle(paste("Threshold accuracy plot for beta =", as.character(b_num))) +
    theme_Publication() +
    scale_colour_Publication() +
    scale_fill_Publication()
  
  ggsave(plot = ggFscore,  
         filename = paste0(out_dir, "fscore",b_char,"_performance_across_thresholds_ABGQI.png"), 
         width = 6, height = 8, units = "in")
  
  print('')
  
}
