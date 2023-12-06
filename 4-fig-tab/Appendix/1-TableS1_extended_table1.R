################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Produce extended table 1 for manuscript looking at effect modification on diarrhea
################################################################################################

rm(list = ls())

# Configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

# Load data ---------------------------------------------------------------
if (run_on_cluster) {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr_offset.RDS")
} else {
  diarr_df_path = paste0(box_data_path, merged_d_diarr_filename)
}

d_diarrhea_pooled = readRDS(diarr_df_path) %>% 
  filter(tr != "Nutrition")  %>% 
  mutate(tr_arm = tr, 
         tr = ifelse(tr == "Control", "Control", "Pooled"),
         tr = as.factor(tr), 
         child_id = paste0(dataid, "_", childid))

# Produce table 1 estimates
calculate_table1_estimates = function(data, var_name, keep_se = F) { 
  estimates = data.frame(washb_mean(data[[var_name]], id = data$clusterid, print = F))
  
  if (keep_se) {
    estimates = estimates %>% select(Mean, Robust.SE) 
  } else {
    estimates = estimates %>% 
      modify_if(is.numeric, ~round(. , 2)) %>% 
      mutate(est = glue("{Mean} ({Lower.95.CI} - {Upper.95.CI})")) %>% 
      select(est)
    colnames(estimates) = c(var_name)
  }
  return (estimates)
}

# -------------------------------------------------------------------------------------
# @summary Function calculates the yearly averages of a variable in a given dataset
# -------------------------------------------------------------------------------------
yearly_averages = function(data, var_name) {
  cis = data[[var_name]] %>% colMeans() %>% t() %>% as.data.frame() %>% 
    mutate(ci_lb = Mean - qnorm(0.975) * Robust.SE, 
           ci_ub = Mean + qnorm(0.975) * Robust.SE, 
           Mean = ifelse(str_detect(var_name, "prop"), Mean * 100, Mean), 
           ci_lb = ifelse(str_detect(var_name, "prop"), ci_lb * 100, ci_lb), 
           ci_ub = ifelse(str_detect(var_name, "prop"), ci_ub * 100, ci_ub)) %>% 
    modify_if(is.numeric, ~round(. , 1)) %>% 
    mutate(est = ifelse(str_detect(var_name, "prop"), glue("{Mean}% ({ci_lb}% - {ci_ub}%)"), glue("{Mean} ({ci_lb} - {ci_ub})"))) %>% 
    select(est)
  colnames(cis) = c(var_name)
  return (cis)
}

n = d_diarrhea_pooled %>% 
  group_by(tr) %>% 
  summarize(n_children = length(unique(child_id)) %>% as.character(),
            n_measurements = n() %>% as.character()) %>% 
  select(-tr) %>% 
  t() %>% 
  as.data.frame() 

prev_diarr = d_diarrhea_pooled %>% 
  mutate(diar7d = as.numeric(diar7d) - 1) %>% 
  group_by(child_id, tr) %>%
  summarize(ever_pos = sum(diar7d > 0)) %>% 
  group_by(tr) %>% 
  summarize(n_ever_pos = sum(ever_pos), 
            prev_ever_pos = mean(ever_pos)*100) %>% 
  modify_if(is.numeric, ~as.character(round(. , 1))) %>% 
  mutate(ever_pos = glue("{n_ever_pos} ({prev_ever_pos}%)")) %>% 
  select(ever_pos) %>% 
  t() %>% 
  as.data.frame()

d_diarrhea_pooled = d_diarrhea_pooled %>% 
  group_by(child_id) %>% 
  arrange(date) %>% 
  mutate(measurement_num = 1:n()) %>% 
  ungroup() 

demog_first_meas = d_diarrhea_pooled %>% 
  filter(measurement_num == 1) %>% 
  group_by(tr) %>% 
  summarize(mean_age_at_first_measurement = calculate_table1_estimates(cur_data() , "agey")) %>% 
  select(-tr) %>% 
  t() %>% 
  as.data.frame()

demog_second_meas = d_diarrhea_pooled %>% 
  filter(measurement_num == 2) %>% 
  group_by(tr) %>% 
  summarize(mean_age_at_second_measurement = calculate_table1_estimates(cur_data() , "agey")) %>% 
  select(-tr) %>% 
  t() %>% 
  as.data.frame()

child_demog = bind_rows(demog_first_meas, demog_second_meas)

hh_demog = d_diarrhea_pooled %>% 
  group_by(child_id) %>% 
  slice_min(date) %>% 
  ungroup() %>% 
  group_by(qgpslong, qgpslat, tr, clusterid) %>% 
  summarize(n_children_in_hh = n()) %>% 
  group_by(tr) %>% 
  summarize(n_hh = n(), 
            mean_children_per_hh = calculate_table1_estimates(cur_data(), "n_children_in_hh")) %>% 
  select(-tr) %>% 
  t() %>% 
  as.data.frame()

tr_arm_counts = d_diarrhea_pooled %>% 
  filter(tr == "Pooled") %>% 
  group_by(tr_arm) %>% 
  summarize(n = as.character(n())) %>% 
  mutate(V1 = "-") %>% 
  tibble::column_to_rownames("tr_arm") %>% 
  select(V1, V2 = n) 

season_tbl = d_diarrhea_pooled %>% 
  filter(year(date) != 2012, year(date) != 2016) %>% 
  group_by(tr) %>% 
  summarize(n_season = sum(season == "yes"),
            prop_season = round(n_season / n() * 100, 2),
            season = glue("{n_season} ({prop_season}%)")) %>% 
  select(season) %>% 
  t() %>% 
  as.data.frame()

make_environmental_var_tbl = function(lag_period){
  environmental_vars = d_diarrhea_pooled %>% 
    filter(year(date) != 2012, year(date) != 2016) %>% 
    group_by(tr, year(date)) %>% 
    summarize(mean_avgtemp = calculate_table1_estimates(cur_data(), glue::glue("temp_avg_{lag_period}"), keep_se = T), 
              mean_mintemp = calculate_table1_estimates(cur_data(), glue::glue("temp_min_{lag_period}"), keep_se = T),
              mean_maxtemp = calculate_table1_estimates(cur_data(), glue::glue("temp_max_{lag_period}"), keep_se = T), 
              mean_ppt_week_sum = calculate_table1_estimates(cur_data(), glue::glue("ppt_sum_{lag_period}"), keep_se = T)) %>% 
    group_by(tr) %>% 
    summarize(mean_avgtemp = yearly_averages(cur_data(), glue::glue("mean_avgtemp")),
              mean_mintemp = yearly_averages(cur_data(), glue::glue("mean_mintemp")),
              mean_maxtemp = yearly_averages(cur_data(), glue::glue("mean_maxtemp")),
              mean_ppt_week_sum = yearly_averages(cur_data(), glue::glue("mean_ppt_week_sum"))) %>% 
    select(-tr) %>% 
    t() %>% 
    as.data.frame()
  
  environmental_vars_categorical = d_diarrhea_pooled %>% 
    filter(year(date) != 2012, year(date) != 2016) %>% 
    group_by(tr) %>% 
    summarize(n_heavyrain = sum(!!sym(glue::glue("heavyrain_{lag_period}")) == "yes", na.rm = T),
              prop_heavyrain = round(n_heavyrain / n() * 100, 2),
              heavyrain = glue("{n_heavyrain} ({prop_heavyrain}%)"),
              
              n_ppt_sum_median = sum(!!sym(glue::glue("ppt_sum_{lag_period}_median")) == "yes", na.rm = T),
              prop_ppt_sum_median = round(n_ppt_sum_median / n() * 100, 2),
              ppt_sum_median = glue("{n_ppt_sum_median} ({prop_ppt_sum_median}%)"),
              
              n_temp_avg_median = sum(!!sym(glue::glue("temp_avg_{lag_period}_median")) == "yes", na.rm = T),
              prop_temp_avg_median = round(n_temp_avg_median / n() * 100, 2),
              temp_avg_median = glue("{n_temp_avg_median} ({prop_temp_avg_median}%)"),
              
              n_temp_min_median = sum(!!sym(glue::glue("temp_min_{lag_period}_median")) == "yes", na.rm = T),
              prop_temp_min_median = round(n_temp_min_median / n() * 100, 2),
              temp_min_median = glue("{n_temp_min_median} ({prop_temp_min_median}%)"),
              
              n_temp_max_median = sum(!!sym(glue::glue("temp_max_{lag_period}_median")) == "yes", na.rm = T),
              prop_temp_max_median = round(n_temp_max_median / n() * 100, 2),
              temp_max_median = glue("{n_temp_max_median} ({prop_temp_max_median}%)")) %>% 
    select(heavyrain, ppt_sum_median, temp_avg_median, temp_min_median, temp_max_median) %>% 
    t() %>% 
    as.data.frame()
  
  missingness = d_diarrhea_pooled %>% 
    filter(year(date) != 2012, year(date) != 2016) %>% 
    group_by(tr) %>% 
    summarise(n_ppt_sum_missing = sum(is.na(!!sym(glue::glue("ppt_sum_{lag_period}_median")))),
              prop_ppt_sum_missing = round(n_ppt_sum_missing / n() * 100, 2),
              ppt_sum_missing = glue("{n_ppt_sum_missing} ({prop_ppt_sum_missing}%)"),
              
              n_temp_avg_missing = sum(is.na(!!sym(glue::glue("temp_avg_{lag_period}_median")))),
              prop_temp_avg_missing = round(n_temp_avg_missing / n() * 100, 2),
              temp_avg_missing = glue("{n_temp_avg_missing} ({prop_temp_avg_missing}%)")) %>% 
    select(ppt_sum_missing, temp_avg_missing) %>% 
    t() %>% 
    as.data.frame()

  rownames(environmental_vars) = paste0(rownames(environmental_vars), "_", lag_period)
  rownames(environmental_vars_categorical) = paste0(rownames(environmental_vars_categorical), "_", lag_period)
  rownames(missingness) = paste0(rownames(missingness), "_", lag_period)
  
  return(bind_rows(environmental_vars, environmental_vars_categorical, missingness))
}

env_tables = lapply(paste0(0:3, "weeklag"), make_environmental_var_tbl) %>% bind_rows()
# Build table 1 --------------------------------------------------------------------
table1 = bind_rows(
  n, 
  hh_demog,
  child_demog,
  prev_diarr, 
  tr_arm_counts,
  season_tbl,
  env_tables
) 

colnames(table1) = c("Control", "Any WASH Intervention")
table1$Variable = rownames(table1)
rownames(table1) = NULL

# Clean table 1 variable names --------------------------------------------------------------------
table1 = table1 %>% 
  mutate(Variable = case_when(Variable == "n_children" ~ "Children",
                              Variable == "n_measurements" ~ "Observations",
                              Variable == "n_hh" ~ "Households",
                              Variable == "mean_children_per_hh" ~ "Children per household",
                              Variable == "ever_pos" ~ "Prevalence",
                              Variable == "mean_age_at_first_measurement" ~ "Age at first measurement (years)",
                              Variable == "mean_age_at_second_measurement" ~ "Age at second measurement (years)",
                              
                              Variable == "season" ~ "In rainy season",
                              
                              Variable == "mean_avgtemp_0weeklag" ~ "Mean temperature, 0 week lag",
                              Variable == "temp_avg_median_0weeklag" ~ "Mean temperature above median, 0 week lag",
                              Variable == "mean_mintemp_0weeklag" ~ "Minimum temperature, 0 week lag",
                              Variable == "temp_min_median_0weeklag" ~ "Minimum temperature above median, 0 week lag",
                              Variable == "mean_maxtemp_0weeklag" ~ "Maximum temperature, 0 week lag",
                              Variable == "temp_max_median_0weeklag" ~ "Maximum temperature above median, 0 week lag",
                              Variable == "temp_avg_missing_0weeklag" ~ "Missing temperature values, 0 week lag",
                              Variable == "mean_ppt_week_sum_0weeklag" ~ "Total precipitation, 0 week lag",
                              Variable == "ppt_sum_median_0weeklag" ~ "Total preciptiation above median, 0 week lag",
                              Variable == "heavyrain_0weeklag" ~ "Heavy rain (1+ days $\\geq$ 80th percentile), 0 week lag",
                              Variable == "ppt_sum_missing_0weeklag" ~ "Missing precipitation values, 0 week lag",
                              
                              Variable == "mean_avgtemp_1weeklag" ~ "Mean temperature, 1 week lag",
                              Variable == "temp_avg_median_1weeklag" ~ "Mean temperature above median, 1 week lag",
                              Variable == "mean_mintemp_1weeklag" ~ "Minimum temperature, 1 week lag",
                              Variable == "temp_min_median_1weeklag" ~ "Minimum temperature above median, 1 week lag",
                              Variable == "mean_maxtemp_1weeklag" ~ "Maximum temperature, 1 week lag",
                              Variable == "temp_max_median_1weeklag" ~ "Maximum temperature above median, 1 week lag",
                              Variable == "temp_avg_missing_1weeklag" ~ "Missing temperature values, 1 week lag",
                              Variable == "mean_ppt_week_sum_1weeklag" ~ "Total precipitation, 1 week lag",
                              Variable == "ppt_sum_median_1weeklag" ~ "Total preciptiation above median, 1 week lag",
                              Variable == "heavyrain_1weeklag" ~ "Heavy rain (1+ days $\\geq$ 80th percentile), 1 week lag",
                              Variable == "ppt_sum_missing_1weeklag" ~ "Missing precipitation values, 1 week lag",
                              
                              Variable == "mean_avgtemp_2weeklag" ~ "Mean temperature, 2 week lag",
                              Variable == "temp_avg_median_2weeklag" ~ "Mean temperature above median, 2 week lag",
                              Variable == "mean_mintemp_2weeklag" ~ "Minimum temperature, 2 week lag",
                              Variable == "temp_min_median_2weeklag" ~ "Minimum temperature above median, 2 week lag",
                              Variable == "mean_maxtemp_2weeklag" ~ "Maximum temperature, 2 week lag",
                              Variable == "temp_max_median_2weeklag" ~ "Maximum temperature above median, 2 week lag",
                              Variable == "temp_avg_missing_2weeklag" ~ "Missing temperature values, 2 week lag",
                              Variable == "mean_ppt_week_sum_2weeklag" ~ "Total precipitation, 2 week lag",
                              Variable == "ppt_sum_median_2weeklag" ~ "Total preciptiation above median, 2 week lag",
                              Variable == "heavyrain_2weeklag" ~ "Heavy rain (1+ days $\\geq$ 80th percentile), 2 week lag",
                              Variable == "ppt_sum_missing_2weeklag" ~ "Missing precipitation values, 2 week lag",
                              
                              Variable == "mean_avgtemp_3weeklag" ~ "Mean temperature, 3 week lag",
                              Variable == "temp_avg_median_3weeklag" ~ "Mean temperature above median, 3 week lag",
                              Variable == "mean_mintemp_3weeklag" ~ "Minimum temperature, 3 week lag",
                              Variable == "temp_min_median_3weeklag" ~ "Minimum temperature above median, 3 week lag",
                              Variable == "mean_maxtemp_3weeklag" ~ "Maximum temperature, 3 week lag",
                              Variable == "temp_max_median_3weeklag" ~ "Maximum temperature above median, 3 week lag",
                              Variable == "temp_avg_missing_3weeklag" ~ "Missing temperature values, 3 week lag",
                              Variable == "mean_ppt_week_sum_3weeklag" ~ "Total precipitation, 3 week lag",
                              Variable == "ppt_sum_median_3weeklag" ~ "Total preciptiation above median, 3 week lag",
                              Variable == "heavyrain_3weeklag" ~ "Heavy rain (1+ days $\\geq$ 80th percentile), 3 week lag",
                              Variable == "ppt_sum_missing_3weeklag" ~ "Missing precipitation values, 3 week lag",
                              
                              T ~ Variable)) %>% 
  select(Variable, "Any WASH Intervention", "Control")

table1$Variable = factor(table1$Variable,
                         levels = c("Children", "Observations", "Households", "Children per household",
                                    "Age at first measurement (years)", "Age at second measurement (years)",
                                    "Prevalence",
                                    "Water", "Sanitation", "Handwashing", "Combined WSH",
                                    
                                    "Mean temperature, 0 week lag", "Mean temperature above median, 0 week lag",
                                    "Minimum temperature, 0 week lag", "Minimum temperature above median, 0 week lag",
                                    "Maximum temperature, 0 week lag", "Maximum temperature above median, 0 week lag",
                                    "Missing temperature values, 0 week lag",
                                    
                                    "Mean temperature, 1 week lag", "Mean temperature above median, 1 week lag",
                                    "Minimum temperature, 1 week lag", "Minimum temperature above median, 1 week lag",
                                    "Maximum temperature, 1 week lag", "Maximum temperature above median, 1 week lag",
                                    "Missing temperature values, 1 week lag",
                                    
                                    "Mean temperature, 2 week lag", "Mean temperature above median, 2 week lag",
                                    "Minimum temperature, 2 week lag", "Minimum temperature above median, 2 week lag",
                                    "Maximum temperature, 2 week lag", "Maximum temperature above median, 2 week lag",
                                    "Missing temperature values, 2 week lag",
                                    
                                    "Mean temperature, 3 week lag", "Mean temperature above median, 3 week lag",
                                    "Minimum temperature, 3 week lag", "Minimum temperature above median, 3 week lag",
                                    "Maximum temperature, 3 week lag", "Maximum temperature above median, 3 week lag",
                                    "Missing temperature values, 3 week lag",
                                    
                                    "In rainy season", 
                                    
                                    "Total precipitation, 0 week lag", "Total preciptiation above median, 0 week lag", 
                                    "Heavy rain (1+ days $\\geq$ 80th percentile), 0 week lag",
                                    "Missing precipitation values, 0 week lag",
                                    
                                    "Total precipitation, 1 week lag", "Total preciptiation above median, 1 week lag", 
                                    "Heavy rain (1+ days $\\geq$ 80th percentile), 1 week lag",
                                    "Missing precipitation values, 1 week lag",
                                    
                                    "Total precipitation, 2 week lag", "Total preciptiation above median, 2 week lag", 
                                    "Heavy rain (1+ days $\\geq$ 80th percentile), 2 week lag",
                                    "Missing precipitation values, 2 week lag",
                                    
                                    "Total precipitation, 3 week lag", "Total preciptiation above median, 3 week lag", 
                                    "Heavy rain (1+ days $\\geq$ 80th percentile), 3 week lag",
                                    "Missing precipitation values, 3 week lag"))
diarrhea_table1 = table1 %>% arrange(Variable)

# Saves Table 1, which looks at effect modification on diarrhea
write.csv(diarrhea_table1, here::here(tables_dir, "Appendix", "TableS1_Table1_Extended.csv"))
