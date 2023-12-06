################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment
# Configured to be run on Sherlock virtual machine
# Calculate distance from each measurement to the nearest weather station
################################################################################################
rm(list = ls())
# ---------------------------------------------------------------
# Configure directories, load libraries and base functions
# ---------------------------------------------------------------
source(paste0(here::here(), "/0-config.R"))
library(geosphere)

run_on_cluster = F
# ---------------------------------------------------------------
# Load diarrhea data from Box
# ---------------------------------------------------------------
if (run_on_cluster) {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr_offset.RDS")
} else {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr.RDS")#paste0(box_data_path, merged_d_diarr_filename)
}

d_diarrhea = readRDS(diarr_df_path) 

d_diarrhea_pooled = d_diarrhea %>% 
  filter(tr != "Nutrition") %>% 
  mutate(tr = ifelse(tr == "Control", "Control", "Pooled"),
         tr = as.factor(tr))

hh_ll = d_diarrhea_pooled %>% select(qgpslong, qgpslat)

#https://www.ncdc.noaa.gov/cdo-web/datatools/findstation
station_ll = 
  data.frame(name = c("BOGRA", "ISHURDI", "TEJGAON", "AGARTALA", "OSMANY", "CHERRAPUNJI", "DHUBRI"), 
             qgpslong = c(89.368286, 89.049683, 90.384521, 91.252441, 91.867676, 91.733093, 89.983521), 
             qgpslat = c(24.851550, 24.151766, 23.780318, 23.880815, 24.962405, 25.249664, 26.017298))

station_distances = distm(hh_ll %>% select(qgpslong, qgpslat), station_ll %>% select(qgpslong, qgpslat), fun = distHaversine)

min_distances = apply(station_distances, 1, min)
mean(min_distances) / 1000






