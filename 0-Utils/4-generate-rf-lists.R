################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Function to generate a list of environmental risk factors under the specified lag periods
################################################################################################

generate_rf_lists = function(lag_periods){
  risk_factor_list_continuous = 
    c(glue::glue("temp_avg_{lag_periods}lag") %>% as.character(),
      glue::glue("temp_min_{lag_periods}lag") %>% as.character(),
      glue::glue("temp_max_{lag_periods}lag") %>% as.character(),
      glue::glue("ppt_sum_{lag_periods}lag") %>% as.character()) %>%
    as.list()
  
  risk_factor_list_categorical = 
    c("season", 
      glue::glue("heavyrain_{lag_periods}lag") %>% as.character(),
      glue::glue("heavyrain_p90_{lag_periods}lag") %>% as.character(),
    
      glue::glue("ppt_sum_{lag_periods}lag_median") %>% as.character(),
      glue::glue("temp_avg_{lag_periods}lag_median") %>% as.character(),
      glue::glue("temp_min_{lag_periods}lag_median") %>% as.character(),
      glue::glue("temp_max_{lag_periods}lag_median") %>% as.character()) %>% 
    as.list()
  
  return(list(continuous = risk_factor_list_continuous, categorical = risk_factor_list_categorical))
}
