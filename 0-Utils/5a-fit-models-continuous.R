################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Functions to fit GAMs and generate estimates + CIs
################################################################################################

# fit continuous models with gam ---------------------------------------------------------------
fit_gam_continuous <- function(data, yname, risk_factor, contrast, adjust = F){
  data$tr = as.factor(data$tr)
  d_model = data %>% filter(tr %in% contrast, !is.na(!!sym(risk_factor)))
  
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
  
  gam_fit = gamm4(formula_gamm4, 
                  family = "binomial",
                  data = d_model,
                  REML = T,
                  na.action = na.exclude, 
                  random = ~(1|clusterid))
  
  return(gam_fit)
}


predict_continuous_gam <- function(gamfit, risk_factor, adjust_factor = NA, yname, contrast, data, scale){
  print(paste0(contrast,collapse = " vs. "))
  data = data %>% filter(tr %in% contrast) 
  
  # TEMP
  data = data %>% filter(!is.na(!!sym(risk_factor)))
  
  if (!is.na(adjust_factor)){
    newdat_tx1 = data %>% group_by(clusterid) %>%
      mutate(tr %in% contrast[1]) %>% 
      dplyr::select(tr, clusterid, !!sym(yname), !!sym(risk_factor), !!sym(adjust_factor)) %>% 
      filter(!is.na(!!sym(yname)))
    newdat_tx2 = data %>% group_by(clusterid) %>%
      mutate(tr %in% contrast[2]) %>% 
      dplyr::select(tr, clusterid, !!sym(yname), !!sym(risk_factor), !!sym(adjust_factor)) %>% 
      filter(!is.na(!!sym(yname)))
  } else {
    newdat_tx1 = data %>% group_by(clusterid) %>%
      mutate(tr %in% contrast[1]) %>% 
      dplyr::select(tr, clusterid, !!sym(yname), !!sym(risk_factor)) %>% 
      filter(!is.na(!!sym(yname)))
    newdat_tx2 = data %>% group_by(clusterid) %>%
      mutate(tr %in% contrast[2]) %>% 
      dplyr::select(tr, clusterid, !!sym(yname), !!sym(risk_factor)) %>% 
      filter(!is.na(!!sym(yname)))
  }
  
  
  fitp_tx1_ci <- gamCI(m=gamfit,newdata=newdat_tx1,nreps=10000)
  fitp_tx2_ci <- gamCI(m=gamfit,newdata=newdat_tx2,nreps=10000)
  
  # convert linear predictor to prevalance
  fitp_ci <- bind_rows(fitp_tx1_ci,fitp_tx2_ci) %>% 
    mutate(fit = exp(fit)/(1+exp(fit)) * scale,
           lwrS = exp(lwrS)/(1+exp(lwrS)) * scale,
           uprS = exp(uprS)/(1+exp(uprS)) * scale,
           contrast = paste0(contrast,collapse = " vs. ")) 
  
  return(fitp_ci)
  
}

#----------------------------------
# simulataneous CIs for GAMs
# estimated by resampling the 
# Bayesian posterior estimates of
# the variance-covariance matrix
# assuming that it is multivariate normal
# the function below also estimates 
# the unconditional variance-covariance
# matrix, Vb=vcov(x,unconditional=TRUE), 
# which allows for undertainty in the actual
# estimated mean as well 
# (Marra & Wood 2012 Scandinavian Journal of Statistics, 
#  Vol. 39: 53-74, 2012, doi: 10.1111/j.1467-9469.2011.00760.x )
# simultaneous CIs provide much better coverage than pointwise CIs
# see: http://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
#
# @param       m : GAM model fit object from mgcv gam()
# @param newdata : data.frame on which to make predictions from the model fit.
#                  Must include all variables used to fit the model m
# @param  nreps  : number of replications to sample from the Bayesian posterior
#
# @returns : gamCI returns a data.frame identical to newdata with 6 new variables:
#            NOTE: ALL PREDICTIONS ARE ON THE SCALE OF LINEAR PREDICTIONS FROM THE MODEL 
#                 (i.e., log-odds for logit model)
#            fit    : marginal spline prediction for each observation
#            se_fit : approximate standard error of the spline prediction
#            uprP   : upper pointwise 95% confidence interval
#            lwrP   : lower pointwise 95% confidence interval
#            uprS   : upper simultaneous 95% confidence interval
#            lwrS   : lower simultaneous 95% confidence interval
#----------------------------------

gamCI <- function(m,newdata,nreps=10000) {
  require(mgcv)
  require(dplyr)
  Vb <- vcov(m,unconditional = TRUE)
  pred <- predict(m, newdata, se.fit = TRUE)
  fit <- pred$fit
  se.fit <- pred$se.fit
  BUdiff <- MASS::mvrnorm(n=nreps, mu = rep(0, nrow(Vb)), Sigma = Vb)
  Cg <- predict(m, newdata, type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  masd <- apply(absDev, 2L, max)
  crit <- quantile(masd, prob = 0.95, type = 8)
  pred <- data.frame(newdata,fit=pred$fit,se_fit=pred$se.fit)
  pred <- mutate(pred,
                 uprP = fit + (2 * se.fit),
                 lwrP = fit - (2 * se.fit),
                 uprS = fit + (crit * se.fit),
                 lwrS = fit - (crit * se.fit)
  )
  return(pred)
}





