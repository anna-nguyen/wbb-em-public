################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Functions to generate TMLE estimates + CIs
################################################################################################

fit_categorical <- function(data, risk_factor_name, yname, contrast, adjust = F){
  # Convert outcome variable (ex: diar7d) to a binary variable named "out"
  # diar7d is a factor, convert to a numeric where 1 = diarrhea and 0 = no diarrhea
  print(risk_factor_name)
  data = data %>% mutate(out = ifelse(!!sym(yname) == 1, T, F),
                         !!sym(yname) := as.numeric(!!sym(yname)) - 1)
  
  if(adjust){
    if(risk_factor_name == "season") {
      adjust_factor = "temp_avg_1weeklag"
    } else {
      lag_period = str_split(str_remove(risk_factor_name, "_median"), "_")[[1]]
      lag_period = lag_period[length(lag_period)]
      adjust_factor = ifelse(str_detect(risk_factor_name, "temp"), "ppt_sum_", "temp_avg_")
      adjust_factor = paste0(adjust_factor, lag_period)
    }
  }
  
  # Split data by risk factor level
  d1 <- data %>% filter(!!sym(risk_factor_name) == "yes") 
  d0 <- data %>% filter(!!sym(risk_factor_name) == "no")
  
  if(adjust){
    fit1 <- tryCatch({washb::washb_tmle(
      Y = d1 %>% pull(out),
      tr = d1$tr,
      W = d1 %>% select(!!sym(adjust_factor)),
      pval = 1, 
      id = d1$block,
      pair = d1$block,
      family = "binomial",
      contrast = contrast,
      print = F
    )}, error = function(cond){
      return(NA)
    })
    
    fit0 <- tryCatch({washb::washb_tmle(
      Y = d0 %>% pull(out),
      tr = d0$tr,
      W = d0 %>% select(!!sym(adjust_factor)),
      pval = 1, 
      id = d0$block,
      pair = d0$block,
      family = "binomial",
      contrast = contrast,
      print = F
    )}, error = function(cond){
      return(NA)
    })
  } else {
    # Run TMLE for each risk factor level 
    fit1 <- tryCatch({washb::washb_tmle(
      Y = d1 %>% pull(out),
      tr = d1$tr,
      id = d1$block,
      pair = d1$block,
      family = "binomial",
      contrast = contrast,
      print = F
    )}, error = function(cond){
      return(NA)
    })
    
    fit0 <- tryCatch({washb::washb_tmle(
      Y = d0 %>% pull(out),
      tr = d0$tr,
      id = d0$block,
      pair = d0$block,
      family = "binomial",
      contrast = contrast,
      print = F
    )}, error = function(cond){
      return(NA)
    })
  }
  
  # Return all NA if RR estimates are null, else pull sample sizes, risk in the 
  # intervention/control, and esimtate prevalence ratios
  
  if (is.na(fit1$estimates$RR$psi)) {
    fit1_RR = data.frame(N = NA, 
                         risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
                         risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA, 
                         psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else if (length(fit1$estimates$RR) == 0) {
    fit1_RR = data.frame(N = NA, 
                         risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
                         risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA, 
                         psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else {
    N = nrow(d1 %>% filter(tr %in% contrast))
    df_intervention = d1 %>% filter(tr == contrast[2]) 
    risk_intervention = washb_mean(df_intervention %>% pull(!!sym(yname)), id = df_intervention$clusterid, print = F) %>% as.data.frame()
    
    df_control = d1 %>% filter(tr == contrast[1])
    risk_control = washb_mean(df_control %>% pull(!!sym(yname)), id = df_control$clusterid, print = F) %>% as.data.frame()
    
    fit1_RR = data.frame(N = N, 
                         risk.intervention.psi = risk_intervention$Mean, 
                         risk.intervention.CI1 = risk_intervention$`Lower 95%CI`,
                         risk.intervention.CI2 = risk_intervention$`Upper 95%CI`,
                         risk.control.psi = risk_control$Mean, 
                         risk.control.CI1 = risk_control$`Lower 95%CI`,
                         risk.control.CI2 = risk_control$`Upper 95%CI`) %>% 
      bind_cols(data.frame(t(unlist(fit1$estimates$RR))))
  }
  
  if (is.na(fit0$estimates$RR$psi)) {
    fit0_RR = data.frame(N = NA, 
                         risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
                         risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA, 
                         psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else if (length(fit0$estimates$RR) == 0) {
    data.frame(N = NA, 
               risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
               risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA, 
               psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else {
    N = nrow(d0 %>% filter(tr %in% contrast))
    df_intervention = d0 %>% filter(tr == contrast[2])
    risk_intervention = washb_mean(df_intervention %>% pull(!!sym(yname)), id = df_intervention$clusterid, print = F) %>% as.data.frame()
    
    df_control = d0 %>% filter(tr == contrast[2])
    risk_control = washb_mean(df_control %>% pull(!!sym(yname)), id = df_control$clusterid, print = F) %>% as.data.frame()
    
    fit0_RR = data.frame(N = N, 
                         risk.intervention.psi = risk_intervention$Mean, 
                         risk.intervention.CI1 = risk_intervention$`Lower 95%CI`,
                         risk.intervention.CI2 = risk_intervention$`Upper 95%CI`,
                         risk.control.psi = risk_control$Mean, 
                         risk.control.CI1 = risk_control$`Lower 95%CI`,
                         risk.control.CI2 = risk_control$`Upper 95%CI`) %>% 
      bind_cols(data.frame(t(unlist(fit0$estimates$RR))))
  }
  
  RR_res = bind_rows(fit1_RR, fit0_RR) %>% 
    mutate(risk_factor = risk_factor_name,
           level = c("yes","no"),
           var.psi = NA,
           parameter = "RR")
  
  # Return all NA if ATE estimates are null, else pull prevalence difference estimates
  
  if (is.na(fit1$estimates$ATE$psi)) {
    fit1_ATE = data.frame(N = NA, 
                          risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
                          risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA,
                          psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else if (length(fit1$estimates$ATE) == 0) {
    fit1_ATE = data.frame(N = NA, 
                          risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
                          risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA,
                          psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else {
    fit1_ATE = data.frame(N = NA, 
                          risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
                          risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA) %>% 
      data.frame(t(unlist(fit1$estimates$ATE)))
  }
  
  if (is.na(fit0$estimates$ATE$psi)) {
    fit0_ATE = data.frame(psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else if (length(fit0$estimates$ATE) == 0) {
    fit0_ATE = data.frame(psi = NA, CI1 = NA, CI2 = NA, pvalue = NA, log.psi = NA, var.log.psi = NA)
  } else {
    fit0_ATE = data.frame(N = NA, 
                          risk.intervention.psi = NA, risk.intervention.CI1 = NA, risk.intervention.CI2 = NA,
                          risk.control.psi = NA, risk.control.CI1 = NA, risk.control.CI2 = NA) %>% 
      data.frame(t(unlist(fit0$estimates$ATE)))
  }
  
  RD_res = bind_rows(fit1_ATE, fit0_ATE) %>% 
    mutate(risk_factor = risk_factor_name,
           level = c("yes","no"),
           var.log.psi = NA,
           parameter = "RD")
  
  # Combine prevalence ratio and prevalence difference estimates
  
  out = bind_rows(RR_res, RD_res) %>% 
    mutate(contrast = paste0(rev(contrast), collapse = " vs. "))
  
  
  return(out)
}


