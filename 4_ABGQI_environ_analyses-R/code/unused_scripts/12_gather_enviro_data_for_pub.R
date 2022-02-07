ll = '/projects/tropics/users/cquinn/R_400/'
library(dplyr)
library(tidyr)
library(ggplot2)

library(backports, lib.loc = ll)
library(cowplot, lib.loc = ll)
library(gridExtra, lib.loc = ll)
library(ggpubr, lib.loc = ll)

wd = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/inference_aggregation_nonXC/'
df = read.csv(file = paste0(wd, "averages/site_avg_ABGOQU_fscore_075-ABG_ROIs_removed-withError_sites_removed.csv"))

# create additonal useful metrics
df$rec = as.factor(substr(df$site, 4,5))
df$YY = as.integer(substr(df$site, 10, 11))
df$MM = as.integer(substr(df$site, 12, 13))
df$DD = as.integer(substr(df$site, 14, 15))
df$date = paste0('20', df$YY, "-", df$MM, "-", df$DD)
df$JulDate = julian(as.Date(df$date))


# subset to the appropriate 760 sites used for temporal and geographic analyses
wd2 = '/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/'
road_devo = read.csv(paste0(wd2, 'environmental_ABG_model/GIS_products/s2l_sites_March2021_dist_to_DEVELOPED_roads_meters_joined_final.csv'))

xy_df = road_devo
sites = df$site
xy_sites = xy_df[xy_df$SiteID %in% sites,] # 821 records
xy_duplicated = xy_sites[duplicated(xy_sites$SiteID),] # happen to be the correct duplicates (61 sites)
duplicate_sites = as.character(unique(xy_duplicated$SiteID)) # sites that were duplicates
non_duplicate_sites = as.character(sites[!sites %in% duplicate_sites]) # sites without duplicates (710 sites)
xy_sites = xy_sites[xy_sites$SiteID %in% non_duplicate_sites,]
xy_sites = rbind(xy_sites, xy_duplicated) # 760 sites
site_list = xy_sites$SiteID

# clean duplicate points in road dist
xy_df = road_devo
sites = df$site
xy_sites = xy_df[xy_df$SiteID %in% sites,] # 821 records
xy_duplicated = xy_sites[duplicated(xy_sites$SiteID),] # happen to be the correct duplicates (61 sites)
duplicate_sites = as.character(unique(xy_duplicated$SiteID)) # sites that were duplicates
non_duplicate_sites = as.character(sites[!sites %in% duplicate_sites]) # sites without duplicates (710 sites)
xy_sites = xy_sites[xy_sites$SiteID %in% non_duplicate_sites,]
xy_sites = rbind(xy_sites, xy_duplicated) # 760 sites
road_dist = xy_sites

df = df %>%
  filter(site %in% site_list)
print(length(unique(df$site)))

########################################################
############ BY HOUR PLOTTING aggregated by LCLU #######
lclu = read_excel('/projects/tropics/users/cquinn/s2l/paper0-ABGOQ_classification/results/environmental_ABG_model/GIS_products/site_landcover_area_50mbuff.xlsx')
joined_df = merge(x = df, y = lclu, by.x = 'site', by.y = 'SiteID') # removes 11 sites for n = 760
joined_df = merge(x = joined_df, y = road_dist, by.x = 'site', by.y = 'SiteID')

# select useful vars
mod_df = data.frame('site' = joined_df$site,
                    'n_min' = joined_df$n_min,
                    'LCLU' = as.factor(joined_df$MaxClass),
                    'nearRoad_m' = joined_df$NEAR_DIST,
                    'Recorder' = joined_df$rec,
                    'YY' = joined_df$YY,
                    'MM' = joined_df$MM,
                    'firstDay' = joined_df$DD,
                    'firstDate' = joined_df$date)

# structural order: UD, AB, HB, SH, RW, FO, FC
mod_df$LCLU_full[mod_df$LCLU == "UD"] <- "Urban/Developed"
mod_df$LCLU_full[mod_df$LCLU == "AB"] <- "Agriculture/Barren"
mod_df$LCLU_full[mod_df$LCLU == "HB"] <- "Herbaceous"
mod_df$LCLU_full[mod_df$LCLU == "SH"] <- "Shrubland"
mod_df$LCLU_full[mod_df$LCLU == "RW"] <- "Riparian/Wetland"
mod_df$LCLU_full[mod_df$LCLU == "FO"] <- "Oak/Hardwood Forest"
mod_df$LCLU_full[mod_df$LCLU == "FC"] <- "Conifer Forest"
mod_df$LULC = factor(mod_df$LCLU_full, levels = c("Urban/Developed",
                                                       "Agriculture/Barren",
                                                       "Herbaceous",
                                                       "Shrubland",
                                                       "Riparian/Wetland",
                                                       "Oak/Hardwood Forest",
                                                       "Conifer Forest"))
mod_df = mod_df %>%
  dplyr::select(-c(LCLU,LCLU_full))

write.csv(mod_df, paste0(wd, 'S2L_site_geog-env_data.csv'), row.names = F)
