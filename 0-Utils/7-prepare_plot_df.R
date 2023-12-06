################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Function to format results ahead of the generation of tables and figures
################################################################################################

prepare_plot_df <- function(df, estimate_type) {
  df %<>% filter(parameter == estimate_type) %>%
    mutate(level = str_to_title(level),
           level = ifelse(str_detect(risk_factor, "median"), 
                          case_when(level == "No" ~ "Below Median", 
                                    level == "Yes" ~ "Above Median"), 
                          level),
           level = ifelse(str_detect(risk_factor, "heavyrain"), 
                          case_when(level == "No" ~ "No Heavy Rain", 
                                    level == "Yes" ~ "Heavy Rain"), 
                          level),
           
           level = ifelse(str_detect(risk_factor, "season"), 
                          case_when(level == "No" ~ "Not rainy season", 
                                    level == "Yes" ~ "Rainy season"), 
                          level),
           category = 
             case_when(str_detect(risk_factor, "season") | str_detect(risk_factor, "ppt") | str_detect(risk_factor, "heavyrain") ~ "Precipitation",
                       str_detect(risk_factor, "temp") ~ "Temperature"))
  
  df$level %<>% as.factor() %>%
    factor(., levels = c("No","Yes", "Not rainy season", "Rainy season", 
                         "Below Median", "Above Median", "No Heavy Rain", "Heavy Rain"))
  
  df$category %<>% as.factor() %>% factor(., levels = c("Temperature", "Precipitation"))
  
  lags = c(paste0(0:3, "weeklag"), paste0(0:3, "monthlag"))
  factor_levels = c("season",
                    glue::glue("heavyrain_{lags}") %>% as.vector(),
                    glue::glue("ppt_sum_{lags}_median") %>% as.vector(),
                    glue::glue("temp_avg_{lags}_median") %>% as.vector(),
                    glue::glue("temp_min_{lags}_median") %>% as.vector(),
                    glue::glue("temp_max_{lags}_median") %>% as.vector())
  
  df$risk_factor %<>% as.factor() %>%
    factor(., levels = factor_levels) 
  
  df = df %<>% arrange(risk_factor)
  
  df %>% mutate(psi = round(psi, 2), 
                CI1 = round(CI1, 2), 
                CI2 = round(CI2, 2))
}