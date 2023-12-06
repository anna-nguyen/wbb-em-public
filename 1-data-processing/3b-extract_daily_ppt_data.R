################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Extract daily precipitation data from rasters
################################################################################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(geosphere)
library(lubridate)
library(future)
library(future.apply)
library(raster)
library(terra)

d_diarr = readRDS(cleaned_d_diarr_filepath) %>% subset_washb_diarr_df() 
ll = d_diarr %>% dplyr::select(qgpslong, qgpslat) %>% filter(!is.na(qgpslong), !is.na(qgpslat)) %>% distinct()
ll_sf = sf::st_as_sf(ll, coords=c("qgpslong","qgpslat"), crs=4326)

ppt_rasters_dir = paste0(box_data_path, "subset_ppt_data/")
ppt_rasters_files = list.files(ppt_rasters_dir)

extract_ppt_values = function(filename){
  file_path = paste0(ppt_rasters_dir, filename)
  ppt_raster_stack = raster::stack(file_path)
  
  ll %>% 
    bind_cols(extract(ppt_raster_stack, ll_sf, df = T, cellnumbers = T)) %>% 
    dplyr::select(-ID) %>% 
    rename(pixelID = cells) %>% 
    melt(id.vars=c("qgpslong", "qgpslat", "pixelID")) %>% 
    mutate(date = str_remove(variable, "X"),
           date = str_replace_all(date, "[.]", "-"), 
           date = as.Date(date)) %>% 
    dplyr::select(qgpslong, qgpslat, pixelID, date, ppt = value)
}

# Runtime: ~50 secs.
tic()
all_ppt = lapply(ppt_rasters_files, extract_ppt_values) %>% bind_rows()
toc()

saveRDS(all_ppt, paste0(box_data_path, "all_ppt_data.RDS"))

# Get values by pixel for cutoff calculations
all_ppt_by_pixel = all_ppt %>% 
  dplyr::select(-qgpslong, -qgpslat) %>% 
  group_by(pixelID, date) %>% 
  slice_head(n = 1)
  
saveRDS(all_ppt_by_pixel, paste0(box_data_path, "all_ppt_data_by_pixelID.RDS"))


