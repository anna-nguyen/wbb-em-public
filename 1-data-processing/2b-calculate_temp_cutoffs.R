################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Calculate cutoffs for categorical temperature variables
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
ll_distinct = ll %>% dplyr::select(qgpslong, qgpslat) %>% distinct()

all_daily_fldas_data = readRDS(paste0(box_data_path, "daily_temperatures_fldas.RDS"))
all_daily_fldas_data = all_daily_fldas_data %>% dplyr::select(cells, temp, date) %>% distinct()

# Calculate cutoffs
future::plan(multisession, workers = 4)

all_7day_temps = furrr::future_map_dfr(unique(all_daily_fldas_data$date), function (d) {
  ret = all_daily_fldas_data %>%
    filter(between(date, d - 7 + 1, d)) %>%
    group_by(cells) %>%
    summarize(absmaxtemp_7_days = max(temp, na.rm = TRUE),
              absmintemp_7_days = min(temp, na.rm = TRUE),
              avgtemp_7_days = mean(temp, na.rm = TRUE),
              n = n()) %>%
    mutate(date = d)
  return(ret)
})

future::plan(sequential)

saveRDS(all_7day_temps, paste0(box_data_path, "all_7day_temps.RDS")) 

median_all_7day_temps = all_7day_temps %>%
  mutate_at(vars(starts_with("abs")), function(x) ifelse(abs(x) == Inf, NA, x)) %>% 
  filter(n == 7) %>%
  ungroup() %>% 
  summarise(absmaxtemp_7_days_median = median(absmaxtemp_7_days, na.rm = T),
            absmintemp_7_days_median = median(absmintemp_7_days, na.rm = T), 
            avgtemp_7_days_median = median(avgtemp_7_days, na.rm = T))

temp_cutoffs = list(absmaxtemp_7_days_median_cutoff = median_all_7day_temps$absmaxtemp_7_days_median,
                    absmintemp_7_days_median_cutoff = median_all_7day_temps$absmintemp_7_days_median,
                    avgtemp_7_days_median_cutoff = median_all_7day_temps$avgtemp_7_days_median)

saveRDS(temp_cutoffs, paste0(box_data_path, "temp_cutoffs.RDS"))
