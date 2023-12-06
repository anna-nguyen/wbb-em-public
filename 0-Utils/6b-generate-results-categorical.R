################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Functions to format results from categorical risk factors
################################################################################################
generate_results_categorical = function(outcome, adjusted = F){
  print(outcome)
  if (adjusted) {
    fit_sanitation_control_list = readRDS(paste0(res_dir, "gamfit/gamfit_",outcome,"_sanitation_control_categorical_adjusted.RDS"))
    fit_water_control_list = readRDS(paste0(res_dir,"gamfit/gamfit_",outcome,"_water_control_categorical_adjusted.RDS"))
    fit_handwashing_control_list = readRDS(paste0(res_dir, "gamfit/gamfit_",outcome,"_handwashing_control_categorical_adjusted.RDS"))
    fit_WSH_control_list = readRDS(paste0(res_dir, "gamfit/gamfit_",outcome,"_combined wsh_control_categorical_adjusted.RDS"))
  } else {
    fit_sanitation_control_list = readRDS(paste0(res_dir, "gamfit/gamfit_",outcome,"_sanitation_control_categorical.RDS"))
    fit_water_control_list = readRDS(paste0(res_dir,"gamfit/gamfit_",outcome,"_water_control_categorical.RDS"))
    fit_handwashing_control_list = readRDS(paste0(res_dir, "gamfit/gamfit_",outcome,"_handwashing_control_categorical.RDS"))
    fit_WSH_control_list = readRDS(paste0(res_dir, "gamfit/gamfit_",outcome,"_combined wsh_control_categorical.RDS"))
  }
  
  fit_list = list(fit_water_control_list %>% bind_rows(),
                  fit_sanitation_control_list %>% bind_rows(),
                  fit_handwashing_control_list %>% bind_rows(),
                  fit_WSH_control_list %>% bind_rows()) %>% bind_rows()
  if (adjusted) {
    saveRDS(fit_list, file = paste0(res_dir, "categorical/tmle_", outcome, "_adjusted.RDS"))
  } else {
    saveRDS(fit_list, file = paste0(res_dir, "categorical/tmle_", outcome, ".RDS"))
  }
}


generate_results_categorical_pooled = function(outcome, adjusted = F){
  print(outcome)
  if (adjusted) {
    fit_pooled_list = readRDS(paste0(res_dir, "gamfit-pooled/gamfit_",outcome,"_pooled_control_categorical_adjusted.RDS")) %>% bind_rows()
    saveRDS(fit_pooled_list, file = paste0(res_dir, "categorical-pooled/tmle_", outcome, "_adjusted.RDS"))
  } else {
    fit_pooled_list = readRDS(paste0(res_dir, "gamfit-pooled/gamfit_",outcome,"_pooled_control_categorical.RDS")) %>% bind_rows()
    saveRDS(fit_pooled_list, file = paste0(res_dir, "categorical-pooled/tmle_", outcome, ".RDS"))
  }
  
}

generate_results_categorical_pooled_neg_control = function(outcome){
  print(outcome)
  fit_pooled_list = readRDS(paste0(res_dir, "gamfit-pooled/neg-control/gamfit_",outcome,"_pooled_control_categorical_adjusted.RDS")) %>% bind_rows()
  
  saveRDS(fit_pooled_list, file = paste0(res_dir, "categorical-pooled/neg-control/tmle_", outcome, ".RDS"))
}

