################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Function to format results for easier reporting in manuscript text
################################################################################################

format_categorical_results = function(tmle_results_df, 
                               rf, 
                               parameter_type = "RR", 
                               contrast_label = "Pooled",
                               parentheses = T){
  
  tmle_results_filtered = tmle_results_df %>% 
    filter(risk_factor == rf, 
           parameter == parameter_type, 
           contrast == paste0(contrast_label, " vs. Control")) 
  
  format_tmle = function(l) {
    level_est = tmle_results_filtered %>% filter(level == l)
    if (parentheses) { 
      return(glue("{round(level_est$psi, 2)} (95% CI {round(level_est$CI1, 2)}-{round(level_est$CI2, 2)})"))
    } else {
      return(glue("{round(level_est$psi, 2)}, 95% CI {round(level_est$CI1, 2)}-{round(level_est$CI2, 2)}"))
    }
  }
  
  formatted_estimates = lapply(tmle_results_filtered$level, format_tmle)
  names(formatted_estimates) = tmle_results_filtered$level
  return(formatted_estimates)
}


