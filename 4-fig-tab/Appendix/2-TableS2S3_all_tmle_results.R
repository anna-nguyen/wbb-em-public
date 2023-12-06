################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Tables with all TMLE prevalence ratio estimates
################################################################################################

# Format tables for TMLE estimates
library(here)
library(tidyverse)
library(writexl)
source(here::here("0-config.R"))

lag_periods = paste0(0:3, "week")

for (lag_period in lag_periods) {
  #--------------------------------------------
  # Load in results
  #--------------------------------------------
  tmle_diarrhea <- readRDS(paste0(res_dir,"categorical/tmle_diarrhea_adjusted.RDS")) %>% 
    filter(risk_factor == "season" | str_detect(risk_factor, lag_period))
  
  tmle_diarrhea_pooled <- readRDS(paste0(res_dir,"categorical-pooled/tmle_diarrhea_adjusted.RDS")) %>% 
    filter(risk_factor == "season" | str_detect(risk_factor, lag_period))
  
  tmle_pooled = tmle_diarrhea %>% 
    bind_rows(tmle_diarrhea_pooled) %>% 
  #  prepare_plot_df(estimate_type = "RR") %>% 
    filter(contrast != "Any Water vs. Control")  %>% 
    mutate(contrast = str_remove(contrast, " vs. Control"),
           contrast = ifelse(contrast == "Pooled", "Pooled Intervention", contrast),
           contrast = factor(contrast, levels = c("Pooled Intervention", "Combined WSH", "Water", "Sanitation", "Handwashing")), 
           Variable = case_when(str_detect(risk_factor, "avg") ~ "Average Weekly Temperature",
                                str_detect(risk_factor, "min") ~ "Minimum Weekly Temperature",
                                str_detect(risk_factor, "max") ~ "Maximum Weekly Temperature",
                                
                                str_detect(risk_factor, "heavyrain") ~ "Heavy Rain",
                                str_detect(risk_factor, "ppt") ~ "Total Weekly Precipitation",
                                str_detect(risk_factor, "season") ~ "Rainy Season"), 
           Variable = factor(Variable, levels = c("Rainy Season", "Total Weekly Precipitation", "Heavy Rain", "Average Weekly Temperature", "Minimum Weekly Temperature", "Maximum Weekly Temperature")))
  
  
  #--------------------------------------------
  # Prepare each outcome dataset for plotting
  #--------------------------------------------
  tmle_pooled_RR = tmle_pooled %>% 
    mutate(Outcome = "Diarrhea") %>%
    prepare_plot_df(estimate_type = "RR") %>% 
    select(-c(var.psi, Outcome, category, parameter,
              level, risk_factor, pvalue, log.psi, var.log.psi)) %>%
    rename(psi.RR = psi, CI1.RR = CI1, CI2.RR = CI2) %>%
    mutate(N = formatC(N, big.mark=","),
           risk.intervention.psi = sprintf("%0.02f", risk.intervention.psi),
           risk.intervention.CI1 = sprintf("%0.02f", risk.intervention.CI1),
           risk.intervention.CI2 = sprintf("%0.02f", risk.intervention.CI2),
           risk.control.psi = sprintf("%0.02f", risk.control.psi),
           risk.control.CI1 = sprintf("%0.02f", risk.control.CI1),
           risk.control.CI2 = sprintf("%0.02f", risk.control.CI2),
           psi.RR = sprintf("%0.02f", psi.RR),
           CI1.RR = sprintf("%0.02f", CI1.RR),
           CI2.RR = sprintf("%0.02f", CI2.RR))
  
  tmle_pooled_RR = tmle_pooled_RR %>% 
    mutate(risk.intervention.psi.CIS = 
             paste0(tmle_pooled_RR$risk.intervention.psi, " (", 
                    tmle_pooled_RR$risk.intervention.CI1, ", ", tmle_pooled_RR$risk.intervention.CI2, ")"),
           risk.control.psi.CIS = 
             paste0(tmle_pooled_RR$risk.control.psi, " (", 
                    tmle_pooled_RR$risk.control.CI1, ", ", tmle_pooled_RR$risk.control.CI2, ")"),
           psi.RR.CIS = 
             paste0(tmle_pooled_RR$psi.RR, " (", 
                    tmle_pooled_RR$CI1, ", ", tmle_pooled_RR$CI2, ")")) 
  
  tmle_pooled_RD = tmle_pooled %>% prepare_plot_df(estimate_type = "RD") %>% 
    mutate(Outcome = "Diarrhea") %>%
    select(-c(N, Variable, risk.intervention.psi, risk.intervention.CI1, risk.intervention.CI2, 
              risk.control.psi, risk.control.CI1, risk.control.CI2, log.psi, var.log.psi, contrast,
              var.psi, pvalue)) %>%
    rename(psi.RD = psi, CI1.RD = CI1, CI2.RD = CI2) %>%
    mutate(psi.RD = sprintf("%0.02f", psi.RD),
           psi.RD = case_when(psi.RD == -0.00 ~ 0.00, TRUE ~ as.numeric(psi.RD)),
           CI1.RD = sprintf("%0.02f", CI1.RD),
           CI1.RD = case_when(CI1.RD == -0.00 ~ 0.00, TRUE ~ as.numeric(CI1.RD)),
           CI2.RD = sprintf("%0.02f", CI2.RD),
           CI2.RD = case_when(CI2.RD == -0.00 ~ 0.00, TRUE ~ as.numeric(CI2.RD)))
  
  tmle_pooled_RD = tmle_pooled_RD %>% 
    mutate(psi.RD.CIS = paste0(sprintf("%0.02f", round(tmle_pooled_RD$psi.RD,2) + 0), " (",
                               sprintf("%0.02f", round(tmle_pooled_RD$CI1.RD,2) + 0), ", ", sprintf("%0.02f", round(tmle_pooled_RD$CI2.RD,2) + 0), ")"))
  
  #--------------------------------------------
  # Combine RR and RD data frames  
  #--------------------------------------------
  tmle_pooled_RRRD <- bind_cols(tmle_pooled_RR, tmle_pooled_RD) %>%
    select(contrast, category, Variable, level, N, risk.intervention.psi.CIS, risk.control.psi.CIS, psi.RR.CIS, psi.RD.CIS) %>% 
    arrange(contrast)

  #--------------------------------------------
  # Temperature Table
  #--------------------------------------------
  outcome_str = "diarrhea"
  temp_table <- tmle_pooled_RRRD %>%
    filter(category == "Temperature", 
           !str_detect(level, "Heatwave")) %>%
    mutate(lag = paste0(substr(lag_period, 1, 1), " Week Lag")) %>% 
    select(-category) %>%
    select(lag, everything()) %>% 
    rename("Lag Period" = lag, 
           Level = level, 
           "Risk in Intervention Arm" = risk.intervention.psi.CIS, 
           "Risk in Control Arm" = risk.control.psi.CIS, 
           "Risk Ratio (95% CI)" = psi.RR.CIS, 
           "Risk Difference (95% CI)" = psi.RD.CIS) 
  write.csv(temp_table, paste0(tables_dir, glue::glue("Appendix/temperature_{lag_period}_table.csv")))
  
  #--------------------------------------------
  # Precipitation Table
  #--------------------------------------------
  precipitation_table <- tmle_pooled_RRRD %>%
    filter(category == "Precipitation") %>%
    select(-category) %>%
    mutate(lag = paste0(substr(lag_period, 1, 1), " Week Lag")) %>% 
    select(lag, everything()) %>% 
    rename("Lag Period" = lag, 
           Level = level, 
           "Risk in Intervention Arm" = risk.intervention.psi.CIS, 
           "Risk in Control Arm" = risk.control.psi.CIS, 
           "Risk Ratio (95% CI)" = psi.RR.CIS, 
           "Risk Difference (95% CI)" = psi.RD.CIS) 
  write.csv(precipitation_table, paste0(tables_dir, glue::glue("Appendix/precipitation_{lag_period}_table.csv")))
}

lag_periods = 0:3
supplemental_ppt_files = glue::glue("precipitation_{lag_periods}week_table.csv") %>% c()
supplemental_temp_files = glue::glue("temperature_{lag_periods}week_table.csv") %>% c()

lapply(supplemental_ppt_files, function(filename) read.csv(paste0(tables_dir, "Appendix/", filename)) %>% select(-X)) %>% 
  bind_rows() %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/TableS2tmle_ppt_all_lags_tbl.csv")), row.names = F)

lapply(supplemental_temp_files, function(filename) read.csv(paste0(tables_dir, "Appendix/", filename)) %>% select(-X)) %>% 
  bind_rows() %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/TableS3tmle_temp_all_lags_tbl.csv")), row.names = F)

