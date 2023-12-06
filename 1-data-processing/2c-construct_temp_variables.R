################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Construct temperature variables for analysis
################################################################################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(geosphere)
library(lubridate)
library(future)
library(future.apply)
library(raster)
library(tidyr)
d_diarr = readRDS(cleaned_d_diarr_filepath) %>% subset_washb_diarr_df() 


ll = d_diarr %>% dplyr::select(qgpslong, qgpslat, date)
ll_distinct = ll %>% dplyr::select(qgpslong, qgpslat) %>% distinct()

all_daily_fldas_data = readRDS(paste0(box_data_path, "daily_temperatures_fldas.RDS"))

# Calculate lag time averages
week_lag_times = data.frame(num_weeks_lag = c(0, 1, 2, 3),
                            start = c(7, 14, 21, 28),
                            end = c(1, 8, 15, 22))

month_lag_times = data.frame(num_months_lag = c(0, 1, 2, 3),
                             start = c(30, 60, 90, 120),
                             end = c(1, 31, 61, 91))


extract_temp_data = function(ll_row){
  row_date = as.Date(ll_row[["date"]])
  row_long = ll_row[["qgpslong"]]
  row_lat = ll_row[["qgpslat"]]
  
  ll_filter = all_daily_fldas_data %>% filter(qgpslong == row_long, qgpslat == row_lat)
  
  find_avg_temp = function(lag_time){
    date_filter = ll_filter %>% 
      filter(date >= row_date - lag_time[["start"]], 
             date <= row_date - lag_time[["end"]])
    
    avg_min_max_tbl = date_filter %>% group_by(qgpslong, qgpslat) %>% 
      summarise("temp_avg" = mean(temp, na.rm = TRUE),
                "temp_min" = min(temp, na.rm = TRUE),
                "temp_max" = max(temp, na.rm = TRUE)) %>% 
      ungroup() %>% 
      dplyr::select("temp_avg", "temp_min", "temp_max")
    
    return(avg_min_max_tbl)
  }
  
  avg_7day_temp = apply(week_lag_times, 1, find_avg_temp) %>% bind_cols()
  colnames(avg_7day_temp) = c("temp_weekavg_0weeklag",
                              "temp_weekmin_0weeklag",
                              "temp_weekmax_0weeklag",
                              
                              "temp_weekavg_1weeklag",
                              "temp_weekmin_1weeklag",
                              "temp_weekmax_1weeklag",
                              
                              "temp_weekavg_2weeklag",
                              "temp_weekmin_2weeklag",
                              "temp_weekmax_2weeklag",
                              
                              "temp_weekavg_3weeklag",
                              "temp_weekmin_3weeklag",
                              "temp_weekmax_3weeklag")
  
  ll_data = data.frame(ll_row)
  return(bind_cols(ll_data, avg_7day_temp))
}

parallel_ranges = data.frame(start_index = c(1, floor(nrow(ll)/4), floor(nrow(ll)/2), floor(nrow(ll) * 3/4))) %>% 
  mutate(end_index = c(lead(start_index) - 1)) %>% replace_na(list(end_index = nrow(ll)))

parallel_process = function(range) {
  lapply(range[["start_index"]]:range[["end_index"]], FUN = function(x) extract_temp_data(ll[x, ])) %>% bind_rows()
}

temp_data = mclapply(1:4, FUN = function(i) parallel_process(parallel_ranges[i, ]), mc.cores = 4) %>% bind_rows()

saveRDS(temp_data, paste0(box_data_path, "washb-bangladesh-temperature-fldas.RDS"))
