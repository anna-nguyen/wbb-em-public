################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Produce table 1 for manuscript looking at effect modification on diarrhea
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

environmental_vars = d_diarrhea_pooled %>% 
  filter(year(date) != 2012, year(date) != 2016) %>% 
  group_by(tr, year(date)) %>% 
  summarize(mean_avgtemp_1weeklag = calculate_table1_estimates(cur_data(), "temp_avg_1weeklag", keep_se = T), 
            mean_mintemp_1weeklag = calculate_table1_estimates(cur_data(), "temp_min_1weeklag", keep_se = T),
            mean_maxtemp_1weeklag = calculate_table1_estimates(cur_data(), "temp_max_1weeklag", keep_se = T), 
            mean_ppt_week_sum_1weeklag = calculate_table1_estimates(cur_data(), "ppt_sum_1weeklag", keep_se = T)) %>% 
  group_by(tr) %>% 
  summarize(mean_avgtemp_1weeklag = yearly_averages(cur_data(), "mean_avgtemp_1weeklag"),
            mean_mintemp_1weeklag = yearly_averages(cur_data(), "mean_mintemp_1weeklag"),
            mean_maxtemp_1weeklag = yearly_averages(cur_data(), "mean_maxtemp_1weeklag"),
            mean_ppt_week_sum_1weeklag = yearly_averages(cur_data(), "mean_ppt_week_sum_1weeklag")) %>% 
  select(-tr) %>% 
  t() %>% 
  as.data.frame()

environmental_vars_categorical = d_diarrhea_pooled %>% 
  filter(year(date) != 2012, year(date) != 2016) %>% 
  group_by(tr) %>% 
  summarize(n_season = sum(season == "yes"),
            prop_season = round(n_season / n() * 100, 2),
            season = glue("{n_season} ({prop_season}%)"),
            
            n_heavyrain_1weeklag = sum(heavyrain_1weeklag == "yes", na.rm = T),
            prop_heavyrain_1weeklag = round(n_heavyrain_1weeklag / n() * 100, 2),
            heavyrain_1weeklag = glue("{n_heavyrain_1weeklag} ({prop_heavyrain_1weeklag}%)"),
            
            n_ppt_sum_1weeklag_median = sum(ppt_sum_1weeklag_median == "yes", na.rm = T),
            prop_ppt_sum_1weeklag_median = round(n_ppt_sum_1weeklag_median / n() * 100, 2),
            ppt_sum_1weeklag_median = glue("{n_ppt_sum_1weeklag_median} ({prop_ppt_sum_1weeklag_median}%)"),
            
            n_temp_avg_1weeklag_median = sum(temp_avg_1weeklag_median == "yes", na.rm = T),
            prop_temp_avg_1weeklag_median = round(n_temp_avg_1weeklag_median / n() * 100, 2),
            temp_avg_1weeklag_median = glue("{n_temp_avg_1weeklag_median} ({prop_temp_avg_1weeklag_median}%)"),
            
            n_temp_min_1weeklag_median = sum(temp_min_1weeklag_median == "yes", na.rm = T),
            prop_temp_min_1weeklag_median = round(n_temp_min_1weeklag_median / n() * 100, 2),
            temp_min_1weeklag_median = glue("{n_temp_min_1weeklag_median} ({prop_temp_min_1weeklag_median}%)"),
            
            n_temp_max_1weeklag_median = sum(temp_max_1weeklag_median == "yes", na.rm = T),
            prop_temp_max_1weeklag_median = round(n_temp_max_1weeklag_median / n() * 100, 2),
            temp_max_1weeklag_median = glue("{n_temp_max_1weeklag_median} ({prop_temp_max_1weeklag_median}%)")) %>% 
  
  select(season, heavyrain_1weeklag, ppt_sum_1weeklag_median, temp_avg_1weeklag_median, temp_min_1weeklag_median, temp_max_1weeklag_median) %>% 
  t() %>% 
  as.data.frame()

missingness = d_diarrhea_pooled %>% 
  filter(year(date) != 2012, year(date) != 2016) %>% 
  group_by(tr) %>% 
  summarise(n_ppt_sum_1weeklag_missing = sum(is.na(ppt_sum_1weeklag)),
            prop_ppt_sum_1weeklag_missing = round(n_ppt_sum_1weeklag_missing / n() * 100, 2),
            ppt_sum_1weeklag_missing = glue("{n_ppt_sum_1weeklag_missing} ({prop_ppt_sum_1weeklag_missing}%)"),
            
            n_temp_avg_1weeklag_missing = sum(is.na(temp_avg_1weeklag)),
            prop_temp_avg_1weeklag_missing = round(n_temp_avg_1weeklag_missing / n() * 100, 2),
            temp_avg_1weeklag_missing = glue("{n_temp_avg_1weeklag_missing} ({prop_temp_avg_1weeklag_missing}%)")) %>% 
  select(ppt_sum_1weeklag_missing, temp_avg_1weeklag_missing) %>% 
  t() %>% 
  as.data.frame()

# Build table 1 --------------------------------------------------------------------
table1 = bind_rows(
  n, 
  hh_demog,
  child_demog,
  prev_diarr, 
  tr_arm_counts,
  environmental_vars,
  environmental_vars_categorical,
  missingness
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
                              
                              Variable == "mean_avgtemp_1weeklag" ~ "Mean temperature",
                              Variable == "temp_avg_1weeklag_median" ~ "Mean temperature above median",
                              Variable == "mean_mintemp_1weeklag" ~ "Minimum temperature",
                              Variable == "temp_min_1weeklag_median" ~ "Minimum temperature above median",
                              Variable == "mean_maxtemp_1weeklag" ~ "Maximum temperature",
                              Variable == "temp_max_1weeklag_median" ~ "Maximum temperature above median",
                              Variable == "temp_avg_1weeklag_missing" ~ "Missing temperature values",
                              
                              Variable == "season" ~ "In rainy season",
                              Variable == "mean_ppt_week_sum_1weeklag" ~ "Total precipitation",
                              Variable == "ppt_sum_1weeklag_median" ~ "Total preciptiation above median",
                              Variable == "heavyrain_1weeklag" ~ "Heavy rain (1+ days $\\geq$ 80th percentile)",
                              Variable == "ppt_sum_1weeklag_missing" ~ "Missing precipitation values",
                              
                              T ~ Variable)) %>% 
  select(Variable, "Any WASH Intervention", "Control")

table1$Variable = factor(table1$Variable,
                         levels = c("Children", "Observations", "Households", "Children per household",
                                    "Age at first measurement (years)", "Age at second measurement (years)",
                                    "Prevalence",
                                    "Water", "Sanitation", "Handwashing", "Combined WSH",
                                    "Mean temperature", "Mean temperature above median",
                                    "Minimum temperature", "Minimum temperature above median",
                                    "Maximum temperature", "Maximum temperature above median",
                                    "Missing temperature values",
                                    "In rainy season", 
                                    "Total precipitation", "Total preciptiation above median", 
                                    "Heavy rain (1+ days $\\geq$ 80th percentile)",
                                    "Missing precipitation values"))
diarrhea_table1 = table1 %>% arrange(Variable)

# Saves Table 1, which looks at effect modification on diarrhea
write.csv(diarrhea_table1, here::here(tables_dir, "Table1.csv"))
