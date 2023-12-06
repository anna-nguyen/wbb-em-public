################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment
# Configured to be run on Sherlock virtual machine
# Fit GAMs for continuous risk factors, run TMLE for categorical risk factors for diarrhea
################################################################################################

rm(list = ls())
# ---------------------------------------------------------------
# Configure directories, load libraries and base functions
# ---------------------------------------------------------------
source(paste0(here::here(), "/0-config.R"))
run_on_cluster = F
# ---------------------------------------------------------------
# Load diarrhea data from Box
# ---------------------------------------------------------------
if (run_on_cluster) {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr_offset.RDS")
} else {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr.RDS")
}

d_diarrhea = readRDS(diarr_df_path) 

d_diarrhea_pooled = d_diarrhea %>% 
  filter(tr != "Nutrition") %>% 
  mutate(tr = ifelse(tr == "Control", "Control", "Pooled"),
         tr = as.factor(tr))

# ---------------------------------------------------------------
# Function to create fitted continuous models for diarrhea
# ---------------------------------------------------------------
fit_diarrhea_continuous_model = function(contrast, pooled = T, adjust = F){
  print(glue("Fitting a continuous model for diarrhea, {contrast[1]} vs {contrast[2]}"))
  
  if (pooled){
    df = d_diarrhea_pooled
  } else if(contrast[2] == "Any Water") {
    df = d_diarrhea %>% mutate(tr = as.character(tr), tr = ifelse(tr %in% c("Water", "Combined WSH"), "Any Water", tr))
  } else {
    df = d_diarrhea
  }
  
  options(warn = -1)
  fit_model_list <- map(risk_factor_list_continuous, function(x)
    fit_gam_continuous(
      data = df,
      yname = "diar7d",
      risk_factor = x,
      contrast = contrast,
      adjust = adjust
    )
  )
  options(warn = 0)
  
  
  contrast = tolower(contrast)
  contrast[2] = ifelse(contrast[2] == "nutrition + wsh", "NWSH", contrast[2])
  
  names(fit_model_list) = paste0("diarrhea-", risk_factor_list_continuous)
  
  directory = glue("{res_dir}gamfit")
  if(pooled) {directory = paste0(directory, "-pooled")}
  
  filename = glue("gamfit_diarrhea_{contrast[2]}_{contrast[1]}_continuous")
  if(adjust) {filename = paste0(filename, "_adjusted")}
  
  saveRDS(fit_model_list, glue("{directory}/{filename}.RDS"))
}

# ---------------------------------------------------------------
# Function to create fitted categorical models for diarrhea
# ---------------------------------------------------------------
fit_diarrhea_categorical_model = function(contrast, pooled = T, adjust = F){
  print(glue("Fitting a categorical model for diarrhea, {contrast[1]} vs {contrast[2]}"))
  
  if (pooled){
    df = d_diarrhea_pooled
  } else if(contrast[2] == "Any Water") {
    df = d_diarrhea %>% mutate(tr = as.character(tr), tr = ifelse(tr %in% c("Water", "Combined WSH"), "Any Water", tr))
  } else {
    df = d_diarrhea
  }
  
  fit_model_list <- map(risk_factor_list_categorical, function(x)
    fit_categorical(
      data = df,
      yname = "diar7d",
      risk_factor_name = x,
      contrast = contrast,
      adjust = adjust
    )
  )
  
  contrast = tolower(contrast)
  contrast[2] = ifelse(contrast[2] == "nutrition + wsh", "NWSH", contrast[2])
  
  names(fit_model_list) = paste0("diarrhea-",risk_factor_list_categorical)
  
  directory = glue("{res_dir}gamfit")
  if(pooled) {directory = paste0(directory, "-pooled")}
  
  filename = glue("gamfit_diarrhea_{contrast[2]}_{contrast[1]}_categorical")
  if(adjust) {filename = paste0(filename, "_adjusted")}
  
  saveRDS(fit_model_list, glue("{directory}/{filename}.RDS"))
}

# ---------------------------------------------------------------
# Fit models 
# ---------------------------------------------------------------

lag_periods = c("0week", "1week", "2week", "3week")

rf_lists = generate_rf_lists(lag_periods)
risk_factor_list_categorical = rf_lists[["categorical"]]
risk_factor_list_continuous = rf_lists[["continuous"]]

map(pooled_contrast_list, function(contrast) fit_diarrhea_categorical_model(contrast, pooled = T, adjust = T)) 
map(contrast_list, function(contrast) fit_diarrhea_categorical_model(contrast, pooled = F, adjust = T)) 

map(pooled_contrast_list, function(contrast) fit_diarrhea_continuous_model(contrast, pooled = T, adjust = T))
map(contrast_list, function(contrast) fit_diarrhea_continuous_model(contrast, pooled = F, adjust = T))
