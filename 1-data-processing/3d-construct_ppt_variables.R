################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Construct precipitation variables for analysis
################################################################################################

rm(list = ls())
source(paste0(here::here(), "/0-config.R"))


d_diarr = readRDS(cleaned_d_diarr_filepath) %>% subset_washb_diarr_df() 
ll_diarr = d_diarr %>% dplyr::select(qgpslong, qgpslat, date)
ll = ll_diarr %>% distinct()


all_ppt_data = readRDS(paste0(box_data_path, "all_ppt_data.RDS"))
cutoffs = readRDS(paste0(box_data_path, "ppt_cutoffs.RDS"))

heavy_rain_cutoff = cutoffs$heavy_rain_cutoff
heavy_rain_cutoff_90th = cutoffs$heavy_rain_cutoff_90th
sum_7d_ppt_median_cutoff = cutoffs$median_7d_cutoff

lag_times = data.frame(num_weeks_lag = c(0, 1, 2, 3),
                       start = c(7, 14, 21, 28),
                       end = c(1, 8, 15, 22))

extract_ppt_data = function(ll_row){
  # ll_row = ll[1,]
  row_date = as.Date(ll_row[["date"]])
  row_long = ll_row[["qgpslong"]]
  row_lat = ll_row[["qgpslat"]]
  
  ll_filter = all_ppt_data %>% filter(as.numeric(qgpslong) == as.numeric(row_long), as.numeric(qgpslat) == as.numeric(row_lat))
  
  find_sum_7day_ppt = function(lag_time){
    date_filter = ll_filter %>% 
      filter(between(date, 
                     row_date - lag_time[["start"]], 
                     row_date - lag_time[["end"]]))
    
    heavy_rain_ind = sum(date_filter$ppt >= heavy_rain_cutoff) > 0
    heavy_rain_ind_90th = sum(date_filter$ppt >= heavy_rain_cutoff_90th) > 0
    
    return(data.frame(ppt_week_sum = sum(date_filter$ppt, na.rm = T), 
                      heavy_rain = heavy_rain_ind, 
                      heavy_rain_90th = heavy_rain_ind_90th) %>%
             mutate(ppt_week_sum_median = factor(ifelse(ppt_week_sum <= cutoffs$median_7d_cutoff, "at or below median", "above median"), levels = c("at or below median", "above median"))))
  }
  
  sum_7day_ppt = apply(lag_times, 1, find_sum_7day_ppt) %>% bind_cols()
  
  colnames(sum_7day_ppt) = c("ppt_week_sum_0weeklag", "heavyrain_0weeklag", "heavyrain_p90_0weeklag", "ppt_week_sum_median_0weeklag",
                             "ppt_week_sum_1weeklag", "heavyrain_1weeklag", "heavyrain_p90_1weeklag", "ppt_week_sum_median_1weeklag",
                             "ppt_week_sum_2weeklag", "heavyrain_2weeklag", "heavyrain_p90_2weeklag", "ppt_week_sum_median_2weeklag",
                             "ppt_week_sum_3weeklag", "heavyrain_3weeklag", "heavyrain_p90_3weeklag", "ppt_week_sum_median_3weeklag")
  
  ll_data = data.frame(t(ll_row))
  return(bind_cols(ll_data, sum_7day_ppt))
}

processed_ppt_data = apply(ll, 1, extract_ppt_data) %>% bind_rows()
saveRDS(processed_ppt_data, paste0(box_data_path, "washb-bangladesh-ppt-new.RDS"))