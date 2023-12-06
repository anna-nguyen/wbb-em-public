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
  diarr_df_path = paste0(box_data_path, merged_d_diarr_filename)
}

d_diarrhea = readRDS(diarr_df_path) 

d_diarrhea_pooled = d_diarrhea %>% 
  filter(tr != "Nutrition") %>% 
  mutate(tr = ifelse(tr == "Control", "Control", "Pooled"),
         tr = as.factor(tr))

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
      yname = "bruise7d",
      risk_factor_name = x,
      contrast = contrast,
      adjust = adjust
    )
  )
  
  contrast = tolower(contrast)
  contrast[2] = ifelse(contrast[2] == "nutrition + wsh", "NWSH", contrast[2])
  
  names(fit_model_list) = paste0("diarrhea-",risk_factor_list_categorical)
  
  directory = glue("{res_dir}gamfit-pooled/neg-control")
  filename = glue("gamfit_diarrhea_{contrast[2]}_{contrast[1]}_categorical")
  if(adjust) {filename = paste0(filename, "_adjusted")}
  
  saveRDS(fit_model_list, glue("{directory}/{filename}.RDS"))
}

# ---------------------------------------------------------------
# Fit models 
# ---------------------------------------------------------------

lag_periods = c("1week")

rf_lists = generate_rf_lists(lag_periods)
risk_factor_list_categorical = rf_lists[["categorical"]]

map(pooled_contrast_list, function(contrast) fit_diarrhea_categorical_model(contrast, pooled = T, adjust = T)) 

