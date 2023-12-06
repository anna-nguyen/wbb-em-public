################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Plot TMLE estimates for temperature under all lag periods
################################################################################################

library(here)
source(here::here("0-config.R"))

tmle_diarrhea_pooled <- readRDS(paste0(res_dir,"categorical-pooled/tmle_diarrhea_adjusted.RDS")) %>% 
  filter(risk_factor != "season") %>% 
  prepare_plot_df(estimate_type = "RR") %>% 
  mutate(Variable = case_when(str_detect(risk_factor, "heavyrain") ~ "Heavy Rain\n\n", 
                              str_detect(risk_factor, "ppt_sum") ~ "Total Weekly Precipitation\n\n", 
                              str_detect(risk_factor, "temp_min") ~ "Minimum Weekly Temperature",
                              str_detect(risk_factor, "temp_max") ~ "Maximum Weekly Temperature",
                              str_detect(risk_factor, "temp_avg") ~ "Average Weekly Temperature"))

lag_period_strings = tmle_diarrhea_pooled$risk_factor %>% str_remove("_median") %>% str_split("_")

tmle_diarrhea_pooled = tmle_diarrhea_pooled %>% 
  mutate(lag_period = sapply(lag_period_strings, function(s) s[length(s)]),
         lag_period = str_replace(lag_period, "week", " Week"),
         lag_period = str_replace(lag_period, "lag", " Lag"))

tmle_diarrhea_pooled %>% 
  mutate(Variable = str_remove(Variable, "\n\n")) %>% 
  filter(category == "Temperature") %>% 
  select("Lag Period" = lag_period, Variable, level, PR = psi, CI_lb = CI1, CI_ub = CI2) %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/ExcelS9_temp_all_lag_periods.csv")), row.names = F)

color_pal = c("Above Median" = "#c23321",
              "Below Median" = "#e6a294")

shape_pal = c("Above Median" = 16,
              "Below Median" = 15)

temp_tmle_plot = 
  ggplot(tmle_diarrhea_pooled %>% filter(str_detect(Variable, "Temperature")), aes(x = lag_period, y = psi)) +
    facet_wrap(~Variable, scales = "free_x") + 
    geom_hline(yintercept = 1, col = "#A4A5A5", alpha = 0.6) +
    geom_point(aes(col = level, shape = level), position = position_dodge(width = 0.5), size = 2.5) +
    geom_linerange(aes(ymin = CI1, ymax = CI2, col = level), position = position_dodge(width = 0.5), lwd = 1) +
    scale_y_continuous(trans = "log", limits = c(0.2, 4),
                       breaks = c(0.25, 0.5, 1, 2, 4)) +
    scale_x_discrete(limits = rev) +
    scale_color_manual(name = "Level", values = color_pal, limits = rev) +
    scale_shape_manual(name = "Level", values = shape_pal, limits = rev)+
    xlab("") + 
    ylab("Prevalence Ratio with 95% CI") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.background = element_rect(fill="white"), 
          strip.text.x = element_text(size = 10, face = "bold"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    coord_flip()

ggsave(glue("{fig_dir}FigureS4_temp_all_lags.png"), temp_tmle_plot, height = 4, width = 8)
           