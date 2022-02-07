ll = '/projects/tropics/users/cquinn/R_400/'
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix, lib.loc = ll) # Kruskal Wallis
library(readxl, lib.loc = ll)
library(corrplot, lib.loc = ll)

# GG plot guides
scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# location for agg data and results
wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/' #//pop.hpc.nau.edu
by_hour_df = read.csv(paste0(wd,"inference_aggregation_nonXC/averages/site_by_hour_ABGOQU_fscore_075-ABG_ROIs_removed-withError_sites_removed.csv"))

# update names to be more friendly for plotting
oldnames = c("Anthro_bin_mean", "Bio_bin_mean", "Geo_bin_mean", "Other_bin_mean", "Quiet_bin_mean", "forced_q_mean")
newnames = c("Anthrophony", "Biophony", "Geophony",  "Interference", "Quiet", "Unidentified")
by_hour_df = by_hour_df %>%
  drop_na() %>%
  rename_at(vars(oldnames), ~ newnames) %>%
  dplyr::select(Anthrophony,Biophony,Geophony,Interference,Quiet,Unidentified,site,HH,n)
by_hour_df$YY = as.integer(substr(by_hour_df$site, 10, 11))
by_hour_df$MM = as.integer(substr(by_hour_df$site, 12, 13))
by_hour_df$DD = as.integer(substr(by_hour_df$site, 14, 15))
by_hour_df$date = paste0('20', by_hour_df$YY, "-", by_hour_df$MM, "-", by_hour_df$DD)
by_hour_df$DOY = lubridate::yday(by_hour_df$date) # day
by_hour_df$Date = as.Date(paste0(by_hour_df$YY[1],'/', by_hour_df$MM, '/', by_hour_df$DD), format = '%y/%m/%d')
by_hour_df$Date_HH = by_hour_df$Date + hours(by_hour_df$HH)

# LCLU 
lclu = read_excel(paste0(wd, 'environmental_ABG_model/GIS_products/site_landcover_area_50mbuff.xlsx'))

joined_df = merge(x = by_hour_df, y = lclu, by.x = 'site', by.y = 'SiteID') # removes 11 sites
joined_df$MaxClass[joined_df$MaxClass == "UD"] <- "Urban/Developed"
joined_df$MaxClass[joined_df$MaxClass == "AB"] <- "Agriculture/Barren"
joined_df$MaxClass[joined_df$MaxClass == "HB"] <- "Herbaceous"
joined_df$MaxClass[joined_df$MaxClass == "SH"] <- "Shrub"
joined_df$MaxClass[joined_df$MaxClass == "RW"] <- "Riparian/Wetland"
joined_df$MaxClass[joined_df$MaxClass == "FO"] <- "Oak Forest"
joined_df$MaxClass[joined_df$MaxClass == "FC"] <- "Conifer Forest"
joined_df$MaxClass = factor(joined_df$MaxClass, levels = c("Urban/Developed",
                                                       "Agriculture/Barren",
                                                       "Herbaceous",
                                                       "Shrub",
                                                       "Riparian/Wetland",
                                                       "Oak Forest",
                                                       "Conifer Forest"))


# correlations
# OVERALL
temp_df = joined_df %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 


# LULC CORRS
# Urban
temp_df = joined_df %>%
  filter(MaxClass == "Urban/Developed") %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 
# cor.test(temp_df$Biophony, temp_df$Interference, method = 'spearman')

# Ag
temp_df = joined_df %>%
  filter(MaxClass == "Agriculture/Barren") %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 

# Herb
temp_df = joined_df %>%
  filter(MaxClass == "Herbaceous") %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 

# Shrub
temp_df = joined_df %>%
  filter(MaxClass == "Shrub") %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 

# Riparian
temp_df = joined_df %>%
  filter(MaxClass == "Riparian/Wetland") %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 

# Oak
temp_df = joined_df %>%
  filter(MaxClass == "Oak Forest") %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 

# Conifer
temp_df = joined_df %>%
  filter(MaxClass == "Conifer Forest") %>%
  dplyr::select(Anthrophony, Biophony, Geophony, Quiet, Interference, Unidentified)
M = cor(temp_df, method = "spearman")
corrplot.mixed(M) 
