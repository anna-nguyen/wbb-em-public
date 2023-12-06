################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# extract daily temperatures from processed rasters
################################################################################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(geosphere)
library(lubridate)
library(future)
library(future.apply)
library(raster)

d_diarr = readRDS(cleaned_d_diarr_filepath) %>% subset_washb_diarr_df() 

ll = d_diarr %>% dplyr::select(qgpslong, qgpslat, date)
ll_distinct = ll %>% dplyr::select(qgpslong, qgpslat) %>% distinct() %>% filter(!is.na(qgpslong), !is.na(qgpslat))
ll_sf = sf::st_as_sf(ll_distinct, coords=c("qgpslong","qgpslat"), crs="+proj=longlat +datum=WGS84 +no_defs")
 
temp_rasters_dir = paste0(box_data_path, "fldas_temperature_data/") 

temp_rasters_files = list.files(temp_rasters_dir)
temp_rasters_files = temp_rasters_files[str_detect(temp_rasters_files, ".tif")]

extract_fldas_data = function(filename){
  file_path = paste0(temp_rasters_dir, "/", filename)
  temp_raster = raster(file_path, varname = "Tair_f_tavg")
  extracted_temp_vals = extract(temp_raster, ll_sf, df = T, cellnumbers = T)
  colnames(extracted_temp_vals)[3] = "temp"
  
  date = str_extract_all(filename, "[[:digit:]]+")[[1]][2]
  extracted_temp_vals$date = as.Date(glue::glue("{str_sub(date, 1, 4)}-{str_sub(date, 5, 6)}-{str_sub(date, 7, 8)}"))
  
  ll_distinct %>% bind_cols(extracted_temp_vals) %>% dplyr::select(-ID)
}

tic()
all_daily_fldas_data = lapply(temp_rasters_files, extract_fldas_data) %>% bind_rows()
toc()

saveRDS(all_daily_fldas_data, paste0(box_data_path, "daily_temperatures_fldas.RDS"))
