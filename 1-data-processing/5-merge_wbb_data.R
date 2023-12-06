################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Merge trial data with temperature and precipitation variables
################################################################################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

################################
# Load Data
################################
d_diarr = readRDS(cleaned_d_diarr_filepath) %>% 
  filter(!is.na(qgpslong)) %>% 
  mutate(date = as.Date(svydate,"%d%b%Y"))

## We also need to define the seasons for the 80th and 20th percentile values
rainy_season_def = read.csv(paste0(box_data_path, "season_definitions.csv")) %>% 
  rename("year" = "season",
         "season_start" = "start",
         "season_end" = "end") %>%
  mutate(season_start = as.Date(season_start), 
         season_end = as.Date(season_end))

################################
# Precipitation 
# precipitation -> 6c-calculate-total-ppt.R
# monthly_precip -> 6d-calculate-monthly-ppt-totals.R
################################
ppt_cutoffs = readRDS(paste0(box_data_path, "ppt_cutoffs.RDS"))

calculate_ppt_cutoffs = function(var) {
  factor(case_when(var <= ppt_cutoffs$median_7d_cutoff ~ "at or below median",
                   var > ppt_cutoffs$median_7d_cutoff ~ "above median"),
         levels = c("at or below median", "above median"))
}

precipitation_df = readRDS(paste0(box_data_path, "washb-bangladesh-ppt-new.RDS")) %>% 
  distinct() %>% 
  dplyr::select(-glue("ppt_week_sum_median_{0:3}weeklag")) %>%  
  mutate(qgpslong = as.numeric(qgpslong),
         qgpslat = as.numeric(qgpslat)) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(across(.cols = contains("heavyrain"), ~factor(ifelse(. == T, "heavy rain", "not heavy rain"), levels = c("not heavy rain", "heavy rain"))),
         across(.cols = contains("ppt_week_sum"), .fns = calculate_ppt_cutoffs, .names = "{.col}_median"),
         year = year(date)) %>%
  # add in season variable
  left_join(rainy_season_def, by = "year") %>%
  mutate(season = factor(ifelse(date > season_start & date < season_end, 1, 0), 
                         levels = c(0,1), 
                         labels = c("dry", "rainy"))) %>% 
  dplyr::select(-year, -season_start, -season_end)

################################
# Temperature
# temperature -> 7b-import-avg-min-max-temp-data.R
# temperature_weekly -> 7c-import-7dayavg-temp-data.R
################################
temp_cutoffs = readRDS(paste0(box_data_path, "temp_cutoffs.RDS"))

calculate_temp_cutoffs = function(var, measure_type) {
  factor(case_when(var <= (temp_cutoffs[[glue("{measure_type}temp_7_days_median_cutoff")]] - 273.15) ~ "at or below median",
                   var > (temp_cutoffs[[glue("{measure_type}temp_7_days_median_cutoff")]] - 273.15) ~ "above median"),
         levels = c("at or below median", "above median"))
}

temperature_df = readRDS(paste0(box_data_path, "washb-bangladesh-temperature-fldas.RDS")) %>% 
  distinct() %>% 
  mutate(across(.cols = contains("temp"), ~ifelse(abs(.) == Inf, NA, . - 273.15)),
         across(.cols = contains("temp_weekavg"), .fns = function(x) calculate_temp_cutoffs(var = x, measure_type = "avg"), .names = "{.col}_median"),
         across(.cols = contains("temp_weekmin"), .fns = function(x) calculate_temp_cutoffs(var = x, measure_type = "absmin"), .names = "{.col}_median"),
         across(.cols = contains("temp_weekmax"), .fns = function(x) calculate_temp_cutoffs(var = x, measure_type = "absmax"), .names = "{.col}_median")) 

################################
# Merge all data sources
################################

diarr_merged = d_diarr %>% 
  left_join(precipitation_df, by = c("qgpslong", "qgpslat", "date")) %>% 
  left_join(temperature_df, by = c("qgpslong", "qgpslat", "date")) 

diarr_merged = diarr_merged %>%
  subset_washb_diarr_df() %>% 
  prepare_rf_variables()

saveRDS(diarr_merged, merged_d_diarr_filepath)
