################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Function to format results from continuous risk factors
################################################################################################
#--------------------------------------------
# Wrapper function to acquire predictions for continuous outcomes
#--------------------------------------------
generate_results_continuous_pooled = function(outcome, risk_factor, adjusted, yname = "diar7d"){
  print(glue("Outcome: {outcome}, Risk Factor: {risk_factor}\n"))
  
  # Load list of model fits
  if (adjusted){
    fit_pooled_list = readRDS(paste0(res_dir, "gamfit-pooled/gamfit_",outcome,"_pooled_control_continuous_adjusted.RDS"))
    lag_period = str_split(risk_factor, "_")[[1]]
    lag_period = lag_period[length(lag_period)]    
    adjust_factor = ifelse(str_detect(risk_factor, "temp"), "ppt_sum_", "temp_avg_")
    adjust_factor = paste0(adjust_factor, lag_period)
    
    group_means = d_diarrhea_pooled %>% group_by(tr) %>% summarize(mean_adjust_factor = mean(!!sym(adjust_factor), na.rm = T))
    d_diarrhea_pooled = d_diarrhea_pooled %>% left_join(group_means, by = "tr") %>% mutate(!!sym(adjust_factor) := mean_adjust_factor)
  } else {
    fit_pooled_list = readRDS(paste0(res_dir, "gamfit-pooled/gamfit_",outcome,"_pooled_control_continuous.RDS"))
    adjust_factor = NA
  }
  
  outcome_rf_label = paste0(outcome, "-", risk_factor)
  
  # Extract risk factor-specific GAM from list of model fits
  gam_fit = fit_pooled_list %>% pluck(outcome_rf_label, "gam")
  
  # Pull predicted prevalence for each risk factor level from fitted GAMs
  model_pred <- map(
    pooled_contrast_list,
    function(x) predict_continuous_gam(
      gamfit = gam_fit,
      risk_factor = as.character(risk_factor),
      adjust_factor = adjust_factor,
      yname = "diar7d",
      contrast = x,
      data = d_diarrhea_pooled %>% filter(!is.na(diar7d)),
      scale = 100
    ))
  
  if (adjusted) {
    saveRDS(model_pred, file = paste0(res_dir, "continuous-pooled/pred_", outcome, "_", yname, "_", risk_factor, "_adjusted.RDS"))
  } else {
    saveRDS(model_pred, file = paste0(res_dir, "continuous-pooled/pred_", outcome, "_", yname, "_", risk_factor, ".RDS"))
  }
}

calculate_gam_contrasts = function(data, risk_factor, yname = "diar7d", predict_low, predict_high, adjust = T){
  if(adjust){
    lag_period = str_split(risk_factor, "_")[[1]]
    lag_period = lag_period[length(lag_period)]    
    adjust_factor = ifelse(str_detect(risk_factor, "temp"), "ppt_sum_", "temp_avg_")
    adjust_factor = paste0(adjust_factor, lag_period)
    formula_str = glue::glue("{yname} ~ s({risk_factor}, by = tr) + s({adjust_factor})")
  } else {
    formula_str = glue::glue("{yname} ~ s({risk_factor}, by = tr)")
  }
  
  formula_gamm4 = as.formula(formula_str)
  
  new_fit = mgcv::gam(formula_gamm4,
                      family = "binomial",
                      data = data,
                      method = "REML",
                      select = TRUE,
                      na.action=na.exclude,
                      random = ~(1|clusterid))
  
  new_data = expand.grid(c("Pooled", "Control"), c(predict_low, predict_high))
  colnames(new_data) = c("tr", risk_factor)
  new_data[adjust_factor] = mean(data[[adjust_factor]], na.rm = T)
  
  new_data$predictions = predict(new_fit, newdata = new_data, se.fit = FALSE)
  
  new_data = new_data %>% mutate(prev = exp(predictions)/(1 + exp(predictions)))
  
  prev_low = new_data %>% filter(!!sym(risk_factor) == predict_low)
  prev_high = new_data %>% filter(!!sym(risk_factor) == predict_high)
  
  pr_low = prev_low$prev[prev_low$tr == "Pooled"] / prev_low$prev[prev_low$tr == "Control"]
  pr_high = prev_high$prev[prev_high$tr == "Pooled"] / prev_high$prev[prev_high$tr == "Control"]
  
  pr_list = bind_cols(pr_low, pr_high)
  colnames(pr_list) = c("low", "high")
  
  return(pr_list)
}

