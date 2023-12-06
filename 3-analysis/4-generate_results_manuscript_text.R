################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Reproduce values reference in the manuscript examining effect modification of WASH on diarrhea
################################################################################################

rm(list = ls())
set.seed(0)

source(here::here("0-config.R"))
run_on_cluster = F

# Load data
if (run_on_cluster) {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr_offset.RDS")
} else {
  diarr_df_path = paste0(box_data_path, merged_d_diarr_filename)
}

d_diarrhea = readRDS(diarr_df_path) 

d_diarrhea_pooled = d_diarrhea %>% 
  filter(tr != "Nutrition") %>% 
  mutate(tr = ifelse(tr == "Control", "Control", "Pooled"),
         tr = as.factor(tr))

# Load Results
season_defs = read.csv(paste0(box_data_path, "season_definitions.csv")) %>% 
  filter(season %>% between(2013, 2015)) %>% 
  mutate(start_date = format(as.Date(start), "%B %d"),
         end_date = format(as.Date(end), "%B %d"), 
         season_dates = glue("{start_date}-{end_date}"))

temp_cutoffs= readRDS(paste0(box_data_path, "temp_cutoffs.RDS"))
ppt_cutoffs= readRDS(paste0(box_data_path, "ppt_cutoffs.RDS"))

fit_pooled_list = readRDS(paste0(res_dir, "gamfit-pooled/gamfit_diarrhea_pooled_control_continuous_adjusted.RDS"))

tmle_results_pooled = readRDS(paste0(res_dir, "categorical-pooled/tmle_diarrhea_adjusted.RDS"))
tmle_results = readRDS(paste0(res_dir, "categorical/tmle_diarrhea_adjusted.RDS")) %>% 
  mutate(tr = str_remove(contrast, " vs. Control"))

continuous_prs = readRDS(paste0(res_dir, "gam_contrasts/all_PRs_from_gam_fits.RDS")) %>% 
  mutate(low_est = glue::glue("{round(low_pr_est, 2)} (95% CI {round(low_pr_lb, 2)}-{round(low_pr_ub, 2)})"),
         high_est = glue::glue("{round(high_pr_est, 2)} (95% CI {round(high_pr_lb, 2)}-{round(high_pr_ub, 2)})")) %>% 
  select(var, low_est, high_est)

###################################
# Abstract ----
###################################

heavy_rain_tmle = tmle_results_pooled %>% filter(risk_factor == "heavyrain_1weeklag", parameter == "RR") %>% 
  mutate(psi = 1 - psi, CI1 = 1 - CI1, CI2 = 1 - CI2, 
         estimate = glue("{round(psi * 100)}% (95% CI {round(CI2 * 100)}%-{round(CI1 * 100)}%)"))
heavy_rain_est = heavy_rain_tmle %>% filter(level == "yes") %>% pull(estimate)
no_heavy_rain_est = heavy_rain_tmle %>% filter(level == "no") %>% pull(estimate)
glue("WASH interventions reduced diarrhea by {heavy_rain_est} following periods with heavy rainfall vs. {no_heavy_rain_est} following periods without heavy rainfall.")

temp_tmle = tmle_results_pooled %>% filter(risk_factor == "temp_avg_1weeklag_median", parameter == "RR") %>% 
  mutate(psi = 1 - psi, CI1 = 1 - CI1, CI2 = 1 - CI2, 
         estimate = glue("{round(psi * 100)}% (95% CI {round(CI2 * 100)}%-{round(CI1 * 100)}%)"))
temp_above_med_est = temp_tmle %>% filter(level == "yes") %>% pull(estimate)
temp_below_med_est = temp_tmle %>% filter(level == "no") %>% pull(estimate)
glue("WASH interventions reduced diarrhea by {temp_above_med_est} following above-median temperatures vs. {temp_below_med_est} following below-median temperatures")

###################################
# Results ----
###################################

## N sizes ----
n_measurements = d_diarrhea %>% nrow()
n_children = d_diarrhea %>% select(qgpslong, qgpslat, childid) %>% distinct() %>% nrow()
age_range = d_diarrhea$agey %>% range() %>% round(., 1)
age_mean = d_diarrhea$agey %>% mean() %>% round(., 1)
age_sd = d_diarrhea$agey %>% sd() %>% round(., 1)
date_range = d_diarrhea$date %>% range() %>% as.Date() %>% format("%b %d, %Y")
glue("Our analysis included {n_measurements} total diarrhea measurements 
      for {n_children} children between {age_range[1]} and {age_range[2]} years of age 
      (mean {age_mean}, SD {age_sd}) during the period between {date_range[1]} and {date_range[2]}.")

## Precipitation ----
# Distribution 
ppt_median = ppt_cutoffs$median_7d_cutoff %>% round()
ppt_range = d_diarrhea$ppt_sum_1weeklag %>% range() %>% round()
glue("During the study, the total weekly precipitation ranged from {ppt_range[1]} to {ppt_range[2]} mm with a median of {ppt_median} mm.")

# Season dates
season_dates = c(season_defs$season_dates)
names(season_dates) = season_defs$season

glue("Precipitation was highly concentrated during the rainy season, which fell between 
      {season_dates[['2013']]} in 2013, {season_dates[['2014']]} in 2014, and {season_dates[['2015']]} in 2015.")

# Total weekly ppt, pooled + continuous
ppt_p_10 = quantile(d_diarrhea$ppt_sum_1weeklag, 0.1, na.rm = T) %>% as.numeric() %>% round()
ppt_p_90 = quantile(d_diarrhea$ppt_sum_1weeklag, 0.9, na.rm = T) %>% as.numeric() %>% round()

total_ppt_prs = continuous_prs %>% filter(var == "ppt_sum_1weeklag")
glue("The prevalence ratio associated with any WASH intervention was {total_ppt_prs$low_est} at the 10th percentile of total rainfall ({ppt_p_10} mm) 
      vs {total_ppt_prs$high_est} at the 90th percentile ({ppt_p_90} mm).")

# Total weekly ppt, pooled + categorical
total_ppt_prs_tmle = format_categorical_results(tmle_results_df = tmle_results_pooled, rf = "ppt_sum_1weeklag_median")
glue("In measurements with above median weekly total rainfall, we estimated a prevalence ratio of {total_ppt_prs_tmle$yes} for any WASH intervention 
     compared to {total_ppt_prs_tmle$no} in measurements with below median total rainfall. ")

# Season, pooled + categorical
season_prs_tmle = format_categorical_results(tmle_results_df = tmle_results_pooled, rf = "season")
glue("The prevalence ratio for the pooled WASH intervention was {season_prs_tmle$yes} during the rainy season 
     compared to {season_prs_tmle$no} during the dry season ")

# Heavy rain, pooled + categorical
heavyrain_prs_tmle = format_categorical_results(tmle_results_df = tmle_results_pooled, rf = "heavyrain_1weeklag")
glue("The prevalence ratio associated with any WASH intervention was lower following weeks when there was at least one day of 
     heavy rainfall {heavyrain_prs_tmle$yes} compared to when there were no days with heavy rainfall {heavyrain_prs_tmle$no}")

# Season, not pooled + categorical
tr_season_tmle = tmle_results %>% filter(risk_factor == "season", parameter == "RR", level == "yes") %>% 
  mutate(psi = 100 - 100 * psi)
s_h_wsh_above = tr_season_tmle %>% filter(tr != "Water") %>% pull(psi) %>% range() %>% round()
w_above = tr_season_tmle %>% filter(tr == "Water") %>% pull(psi) %>% round()

glue("During the rainy season, the sanitation, handwashing, combined WSH interventions reduced diarrhea prevalence by {s_h_wsh_above[1]}% to {s_h_wsh_above[2]}%, while the water intervention reduced it by {w_above}% ")

# Temperature ----
# Distribution
temp_avg_median = (temp_cutoffs$avgtemp_7_days_median_cutoff - 273.15) %>% round() 
temp_avg_range = d_diarrhea$temp_avg_1weeklag %>% range(na.rm = T) %>% round()

temp_min_median = (temp_cutoffs$absmintemp_7_days_median_cutoff - 273.15) %>% round()
temp_min_range = d_diarrhea$temp_min_1weeklag %>% range(na.rm = T) %>% round()

temp_max_median = (temp_cutoffs$absmaxtemp_7_days_median_cutoff - 273.15) %>% round()
temp_max_range = d_diarrhea$temp_max_1weeklag %>% range(na.rm = T) %>% round()

glue("During the study, the weekly average temperature ranged from {temp_avg_range[1]} to {temp_avg_range[2]}°C (median = {temp_avg_median}°C), 
      the minimum temperature ranged from {temp_min_range[1]} to {temp_min_range[2]}°C (median = {temp_min_median}°C), 
      and the maximum temperature ranged from {temp_max_range[1]} to {temp_max_range[2]}°C (median = {temp_max_median}°C)")

# Average Temperature, pooled + continuous
temp_avg_p_10 = quantile(d_diarrhea$temp_avg_1weeklag, 0.1, na.rm = T) %>% as.numeric() %>% round()
temp_avg_p_90 = quantile(d_diarrhea$temp_avg_1weeklag, 0.9, na.rm = T) %>% as.numeric() %>% round()
temp_avg_prs = continuous_prs %>% filter(var == "temp_avg_1weeklag")

glue("At a lower average temperature of {temp_avg_p_10}°C, the prevalence ratio for any WASH intervention was {temp_avg_prs$low} 
     compared to {temp_avg_prs$high} at a higher temperature of {temp_avg_p_90}°C. ")

# Min + Max Temperature, pooled + continuous
temp_min_p_10 = quantile(d_diarrhea$temp_min_1weeklag, 0.1, na.rm = T) %>% as.numeric() %>% round()
temp_min_p_90 = quantile(d_diarrhea$temp_min_1weeklag, 0.9, na.rm = T) %>% as.numeric() %>% round()
temp_min_prs = continuous_prs %>% filter(var == "temp_min_1weeklag") %>% str_remove("\\)") %>% str_replace(" \\(", ", ")

temp_max_p_10 = quantile(d_diarrhea$temp_max_1weeklag, 0.1, na.rm = T) %>% as.numeric() %>% round()
temp_max_p_90 = quantile(d_diarrhea$temp_max_1weeklag, 0.9, na.rm = T) %>% as.numeric() %>% round()
temp_max_prs = continuous_prs %>% filter(var == "temp_max_1weeklag") %>% str_remove("\\)") %>% str_replace(" \\(", ", ")

glue("We saw similar increases in intervention effectiveness under higher 
      minimum temperatures (PR = {temp_min_prs[2]} at {temp_min_p_10}°C vs PR = {temp_min_prs[3]} at {temp_min_p_90}°C) and 
      maximum temperatures (PR = {temp_max_prs[2]} at {temp_max_p_10}°C vs PR = {temp_max_prs[3]} at {temp_max_p_90}°C).")

# Avg + Min + Max Temperature, pooled + categorical
avg_temp_prs_tmle = format_categorical_results(tmle_results_df = tmle_results_pooled, rf = "temp_avg_1weeklag_median")
min_temp_prs_tmle = format_categorical_results(tmle_results_df = tmle_results_pooled, rf = "temp_min_1weeklag_median")
max_temp_prs_tmle = format_categorical_results(tmle_results_df = tmle_results_pooled, rf = "temp_max_1weeklag_median")

glue("We saw similar trends when comparing above median versus below median measurements and estimated prevalence ratios of 
     {avg_temp_prs_tmle$yes} vs {avg_temp_prs_tmle$no} for average temperatures, 
     {min_temp_prs_tmle$yes} vs {min_temp_prs_tmle$no} for minimum temperatures, and 
     {max_temp_prs_tmle$yes} vs {max_temp_prs_tmle$no} for maximum temperatures.")

# Average temperature, not pooled + categorical
tr_temp_tmle = tmle_results %>% filter(risk_factor == "temp_avg_1weeklag_median", parameter == "RR", level == "yes") %>% 
  mutate(psi = 100 - 100 * psi)
s_h_wsh_est = tr_temp_tmle %>% filter(tr != "Water") %>% pull(psi) %>% range() %>% round()
w_est = tr_temp_tmle %>% filter(tr == "Water") %>% pull(psi) %>% round()

glue("During periods where there were above median weekly average temperatures, we estimated between a 
      {s_h_wsh_est[1]} to {s_h_wsh_est[2]}% reduction in diarrhea under the sanitation, handwashing, and combined WSH interventions 
      compared to a {w_est}% reduction under the water intervention.")

###################################
# Conclusion ----
###################################

heavy_rain_reduction_est = tmle_results_pooled %>% 
  filter(risk_factor == "heavyrain_1weeklag", parameter == "RR", level == "yes") %>% 
  mutate(psi = 100 - 100 * psi) %>% pull(psi) %>% round()

temp_avg_reduction_est = tmle_results_pooled %>% 
  filter(risk_factor == "temp_avg_1weeklag_median", parameter == "RR", level == "yes") %>% 
  mutate(psi = 100 - 100 * psi) %>% pull(psi) %>% round()

glue("We found that receipt of any WASH intervention reduced diarrhea prevalence by {heavy_rain_reduction_est}% following heavy rainfall 
     and {temp_avg_reduction_est}% after above-median temperatures compared to the overall 34% reduction observed in the original trial")
