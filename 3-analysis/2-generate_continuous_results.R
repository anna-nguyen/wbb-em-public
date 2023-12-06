rm(list = ls())

source(here::here("0-config.R"))
library(doParallel)
library(doRNG)

run_on_cluster = F

set.seed(0)
risk_factor_list_continuous = list("ppt_sum_1weeklag", "temp_avg_1weeklag", "temp_min_1weeklag", "temp_max_1weeklag")

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

# Make predictions from fitted model
map(risk_factor_list_continuous,
    function(rf) {
      tryCatch({generate_results_continuous_pooled(outcome = "diarrhea", rf, adjusted = T)},
               error = function(cond){
                 print(glue("ERROR -- {rf}\n"))
               })
    })

# Fit new models from bootstraps samples (resampled at the cluster level) to estimate CIs
cluster_id_df = d_diarrhea %>% dplyr::select(qgpslong, qgpslat, date, dataid, clusterid) %>% distinct()
clusters = cluster_id_df$clusterid %>% unique()

calculate_pr_bootstrap = function(risk_factor, df = d_diarrhea_pooled, bootstrap_reps = 1000) {
  # Calculate the values for which CIs should be calculated
  predict_low = quantile(d_diarrhea[risk_factor], 0.1, na.rm = T) %>% as.numeric()
  predict_high = quantile(d_diarrhea[risk_factor], 0.9, na.rm = T) %>% as.numeric()
  
  # Run bootstrap repetitions, store predictions at the percentile values defined above
  bootstrap_results = 
    lapply(1:bootstrap_reps, 
           function (i) 
              calculate_gam_contrasts(data = data.frame(clusterid = sample(clusters, size = length(clusters), replace = T)) %>% 
                                        left_join(df, by = "clusterid"),
                                      risk_factor = risk_factor,
                                      predict_low = predict_low, 
                                      predict_high = predict_high)) %>% 
      bind_rows()
  
  # Calculate the confidence intervals from the results of the bootstrap
  bootstrapped_cis = 
    data.frame(low_pr_est = mean(bootstrap_results$low),
               low_pr_lb = quantile(bootstrap_results$low, 0.025)[[1]],
               low_pr_ub = quantile(bootstrap_results$low, 0.975)[[1]],
               high_pr_est = mean(bootstrap_results$high),
               high_pr_lb = quantile(bootstrap_results$high, 0.025)[[1]],
               high_pr_ub = quantile(bootstrap_results$high, 0.975)[[1]]) %>% 
    mutate(var = risk_factor, n_bootstraps = bootstrap_reps)
  
  saveRDS(bootstrapped_cis, glue::glue("{res_dir}gam_contrasts/bootstrapped_cis_{risk_factor}.RDS"))
}


# Run bootstrapped predictions for each risk factor
mclapply(risk_factor_list_continuous, calculate_pr_bootstrap, mc.cores = 4) 

# Combine the results of each risk factor into a combined dataframe
lapply(risk_factor_list_continuous, 
        function(risk_factor) readRDS(glue::glue("{res_dir}gam_contrasts/bootstrapped_cis_{risk_factor}.RDS"))) %>% 
  bind_rows() %>% 
  saveRDS(paste0(res_dir, "gam_contrasts/all_PRs_from_gam_fits.RDS"))


