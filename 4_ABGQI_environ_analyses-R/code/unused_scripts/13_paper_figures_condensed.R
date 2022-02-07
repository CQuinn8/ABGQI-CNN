# Paper Figures

ll = '/projects/tropics/users/cquinn/R_400/'
library(dplyr)
library(tidyr)
library(ggplot2)
library(backports, lib.loc = ll)
library(cowplot, lib.loc = ll)
library(gridExtra, lib.loc = ll)
library(ggpubr, lib.loc = ll)

source('/projects/tropics/users/cquinn/s2l/code/paper_0-ABG_classification/color_theme_fxs.R')

wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/inference_aggregation_nonXC/'
by_hour_files = list.files(paste0(wd,"averages/"), pattern = 'site_by_hour*')

# read in all hourly csvs
dfs = list()
for(i in 1:length(by_hour_files)){
  temp_df = read.csv(paste0(wd,"averages/",by_hour_files[i]))[,-1]
  temp_f = rev(strsplit(tools::file_path_sans_ext(by_hour_files[i]), "_")[[1]])[[5]] # grab fscore from filename
  temp_f = strsplit(temp_f,"-")[[1]][[1]]
  temp_df$Fscore = format(as.numeric(temp_f) / 100,nsmall = 2) # add decimal place
  dfs[[i]] = temp_df
}
dfs = do.call("rbind", dfs)

# update names to be more friendly for plotting
oldnames = c("Anthro_bin_mean", "Anthro_bin_var",
             "Bio_bin_mean", "Bio_bin_var",
             "Geo_bin_mean", "Geo_bin_var",
             "Other_bin_mean", "Other_bin_var",
             "Quiet_bin_mean", "Quiet_bin_var",
             "forced_q_mean", "forced_q_var")
newnames = c("Anthro", "Anthro_var",
             "Bio", "Bio_var",
             "Geo", "Geo_var",
             "Interference", "Interference_var",
             "Quiet", "Quiet_var",
             "Unidentified", "Unidentified_var")
by_hour_df = dfs %>%
  drop_na() %>%
  rename_at(vars(oldnames), ~ newnames)

# create additonal useful metrics
by_hour_df$rec = as.factor(substr(by_hour_df$site, 4,5))
by_hour_df$YY = as.integer(substr(by_hour_df$site, 10, 11))


# subset to the appropriate 760 sites used for temporal and geographic analyses
wd2 = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/'
road_devo = read.csv(paste0(wd2, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_DEVELOPED_roads_meters_joined_final.csv'))

xy_df = road_devo
sites = by_hour_df$site
xy_sites = xy_df[xy_df$SiteID %in% sites,] # 821 records
xy_duplicated = xy_sites[duplicated(xy_sites$SiteID),] # happen to be the correct duplicates (61 sites)
duplicate_sites = as.character(unique(xy_duplicated$SiteID)) # sites that were duplicates
non_duplicate_sites = as.character(sites[!sites %in% duplicate_sites]) # sites without duplicates (710 sites)
xy_sites = xy_sites[xy_sites$SiteID %in% non_duplicate_sites,]
xy_sites = rbind(xy_sites, xy_duplicated) # 760 sites
site_list = xy_sites$SiteID

by_hour_df = by_hour_df %>%
  filter(site %in% site_list)
print(length(unique(by_hour_df$site)))

########################################################
############ BY HOUR PLOTTING aggregated by LCLU #######
lclu = read_excel('/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/environmental_ABG_model/GIS_products/site_landcover_area_50mbuff.xlsx')
joined_df = merge(x = by_hour_df, y = lclu, by.x = 'site', by.y = 'SiteID') # removes 11 sites for n = 760

# select useful vars
mod_df = data.frame('site' = joined_df$site,
                    'HH' = joined_df$HH,
                    'Anthrophony' = joined_df$Anthro, 
                    'Biophony' = joined_df$Bio, 
                    'Quiet' = joined_df$Quiet,
                    'Geophony' = joined_df$Geo,
                    'Interference' = joined_df$Interference,
                    'Unidentified' = joined_df$Unidentified,
                    'LCLU' = as.factor(joined_df$MaxClass),
                    'Recorder' = joined_df$rec,
                    'YY' = joined_df$YY,
                    'Fscore' = joined_df$Fscore)

# structural order: UD, AB, HB, SH, RW, FO, FC
mod_df$LCLU_full[mod_df$LCLU == "UD"] <- "Urban/Developed"
mod_df$LCLU_full[mod_df$LCLU == "AB"] <- "Agriculture/Barren"
mod_df$LCLU_full[mod_df$LCLU == "HB"] <- "Herbaceous"
mod_df$LCLU_full[mod_df$LCLU == "SH"] <- "Shrubland"
mod_df$LCLU_full[mod_df$LCLU == "RW"] <- "Riparian/Wetland"
mod_df$LCLU_full[mod_df$LCLU == "FO"] <- "Oak/Hardwood Forest"
mod_df$LCLU_full[mod_df$LCLU == "FC"] <- "Conifer Forest"
mod_df$LCLU_full = factor(mod_df$LCLU_full, levels = c("Urban/Developed",
                                                       "Agriculture/Barren",
                                                       "Herbaceous",
                                                       "Shrubland",
                                                       "Riparian/Wetland",
                                                       "Oak/Hardwood Forest",
                                                       "Conifer Forest"))

# data organization - should yield 6 classes * 7 LCLUs * 24 hrs = 1,008 rows
subset_df = mod_df %>%
  dplyr::select(-YY) %>%
  group_by(HH,LCLU_full) %>%
  dplyr::summarise(across(where(is.numeric), mean)) %>% # mean by hour
  reshape2::melt(id = 1:2)


################################################
################### Figure 6 ###################

# plot each label on a new plot from 0-24 hours % faceted by LCLU
(ggToD_LCLU_facet = 
    subset_df %>% 
    #filter(Recorder == 'am') %>%
    #filter(variable == 'Anthrophony' | variable == 'Biophony' | variable == 'Quiet') %>%
    #filter(variable == 'Biophony' ) %>%
    mutate(value = value*100) %>% 
    ggplot(aes(x = HH, y = value)) +
    scale_colour_Publication() +
    geom_line(aes(colour = variable), size = 1.2, alpha = 0.9) +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank()) +
    scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c("0%", '20%', '40%', '60%')) +
    labs(title = paste0("Percent of time present sound is predicted present for n = ", 
                        length(unique(mod_df$site)), 
                        " sites"),
         subtitle = '* = Unidentified determined by when all other labels are absent',
         x = "Hour of Day [0-24]",
         y = paste0("Percent present")) +
    facet_wrap(~LCLU_full)) # facet based on ABGOQ

ggsave(plot = ggToD_LCLU_facet,  
       filename = paste0(wd, "time_of_day_percent_present_faceted_by_LCLU_F075-ABQ.png"), 
       width = 8, height = 8, units = "in")