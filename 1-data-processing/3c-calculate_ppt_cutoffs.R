################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Calculate cutoffs for categorical precipitation variables
################################################################################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(furrr)

## Read in raw  ppt data 
## This includes daily data for all 64 pixels comprising the study area for the duration of the study period
all_ppt_data = readRDS(paste0(box_data_path, "all_ppt_data_by_pixelID.RDS"))

average_daily_ppts = all_ppt_data %>% filter(ppt > 0) 

heavy_rain_cutoff_80th = as.numeric(quantile(average_daily_ppts$ppt, 0.8))
heavy_rain_cutoff_90th = as.numeric(quantile(average_daily_ppts$ppt, 0.9))

future::plan(multisession, workers = 4)

# Runtime: ~2650 secs/~45 minutes
tic("7d ppt sum")
all_7day_ppts = furrr::future_map_dfr(unique(all_ppt_data$date), function (d) {
  ret = all_ppt_data %>%
    filter(between(date, d - 7 + 1, d)) %>%
    group_by(pixelID) %>%
    summarize(sum_ppt = sum(ppt, na.rm = T),
              n = n()) %>%
    mutate(date = d)
  return(ret)
})
toc()

future::plan(sequential)

saveRDS(all_7day_ppts, paste0(box_data_path, "all_7day_ppts.RDS")) 

##Take result and then, 
# 1) remove anything from the first 6 days because the sum doesn't cover a full 7d
# 2) calculate median over all values
# 3) calculate median over all values > 0

median_7_day_ppts = all_7day_ppts %>%
  filter(n == 7) %>%
  summarise(median_7_day_ppt_sum = median(sum_ppt),
            min_7_day_ppt_sum = min(sum_ppt),
            max_7_day_ppt_sum = max(sum_ppt))

median_7d_cutoff = median_7_day_ppts$median_7_day_ppt_sum

cutoffs = list(heavy_rain_cutoff = heavy_rain_cutoff_80th, 
               heavy_rain_cutoff_90th = heavy_rain_cutoff_90th,
               median_7d_cutoff = median_7d_cutoff)

saveRDS(cutoffs, paste0(box_data_path, "ppt_cutoffs.RDS")) 

