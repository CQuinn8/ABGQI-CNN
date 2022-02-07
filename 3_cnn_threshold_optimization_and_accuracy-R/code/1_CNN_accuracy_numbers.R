# Purpose: Summarize pre-calculated accuracy metrics  
# Requires: model_acc_metric csvs for each class
# Outputs: Table 2; portions of Section 3.1
# Author: Colin Quinn <cq73@nau.edu>

setwd('/published_repo/')

library(dplyr)
library(tidyr)
library(ggplot2)
data_dir = '3_cnn_threshold_optimization_and_accuracy-R/results/performance_fscore_'

# set what level of f(beta) is desired without decimal (i.e., 0.75 is 075)
beta = '075/'
csvs = list.files(paste0(data_dir,beta), pattern = "*model_acc_metrics.csv")

csvs_ls = list()
for(i in seq_along(csvs)){
  print(i)
  df = read.csv(paste0(data_dir, beta, csvs[i]))
  df$label = strsplit(csvs[i],"_")[[1]][1] # soundscape classes
  csvs_ls[[i]] = df
}
df = do.call("rbind", csvs_ls)

# Manual calls to print accuracy metrics
# possible accuracy measures; refer to 0_CNN_threshold_optimization.R for metrics
print(unique(df$X))

(temp = df %>% 
    filter(X == "f050"))%>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "f050") %>%
    summarize(mean(test))) 

(temp = df %>% 
    filter(X == "f075"))%>%
    dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "f075") %>%
    summarize(mean(test))) 

(temp = df %>% 
    filter(X == "F1"))%>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "F1") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "Precision"))%>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "Precision") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "Recall"))%>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "Recall") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "auc"))%>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "auc") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "th"))
(temp = df %>% 
    filter(X == "th") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "Accuracy"))%>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "Accuracy") %>%
    summarize(mean(test))) 
