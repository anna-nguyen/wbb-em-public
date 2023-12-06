################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Function to streamline processing of environmental risk factors
################################################################################################

prepare_rf_variables = function(d){
  d = d %>% 
    filter(tr != "Nutrition", 
           !is.na(diar7d)) %>% 
    mutate(intervention = ifelse(tr == "Control", "Control", "Treatment") %>% as.factor(),
           clusterid = as.factor(clusterid),
           tr = ifelse(tr %in% c("Nutrition + WSH", "WSH"), "Combined WSH", as.character(tr)),
           tr = as.factor(tr),
           
           season = as.factor(ifelse(season == "rainy", "yes", "no")),
           
           across(.cols = contains("heavyrain_"), ~as.factor(ifelse(.x == "heavy rain", "yes", "no"))), 
           across(.cols = contains("_median"), ~as.factor(ifelse(.x == "above median", "yes", "no"))), 

           across(.cols = contains("max") & !contains("median"), ~ifelse(.x == -Inf, NA, .x)), 
           across(.cols = contains("min") & !contains("median") , ~ifelse(.x == Inf, NA, .x)))
  
  colnames(d) = gsub("week_sum", "sum", colnames(d))
  colnames(d) = gsub("weekavg", "avg", colnames(d))
  colnames(d) = gsub("weekmin", "min", colnames(d))
  colnames(d) = gsub("weekmax", "max", colnames(d))
  
  return(d)
}
