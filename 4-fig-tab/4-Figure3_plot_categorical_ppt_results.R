################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Forest plots for TMLE risk ratio estimates of precipitation variables
################################################################################################

library(here)
source(here::here("0-config.R"))

#--------------------------------------------
# Load in results
#--------------------------------------------
tmle_diarrhea <- readRDS(paste0(res_dir,"categorical/tmle_diarrhea_adjusted.RDS")) %>% 
  filter(risk_factor == "season" | str_detect(risk_factor, "1week"))

tmle_diarrhea_pooled <- readRDS(paste0(res_dir,"categorical-pooled/tmle_diarrhea_adjusted.RDS")) %>% 
  filter(risk_factor == "season" | str_detect(risk_factor, "1week"))

tmle_pooled = tmle_diarrhea %>% 
  bind_rows(tmle_diarrhea_pooled) %>% 
  prepare_plot_df(estimate_type = "RR") %>% 
  filter(contrast != "Any Water vs. Control",
         (str_detect(risk_factor, "ppt") | str_detect(risk_factor, "rain") | str_detect(risk_factor, "season"))) %>% 
  mutate(contrast = str_remove(contrast, " vs. Control"),
         contrast = ifelse(contrast == "Pooled", "Any WASH Intervention", contrast),
         contrast = factor(contrast, levels = c("Any WASH Intervention", "Combined WSH", "Water", "Sanitation", "Handwashing")), 
         Variable = case_when(str_detect(risk_factor, "heavyrain") ~ "Heavy Rain\n\n",
                              str_detect(risk_factor, "ppt") ~ "Total Weekly Precipitation\n\n",
                              str_detect(risk_factor, "season") ~ "Rainy Season\n\n"), 
         Variable = factor(Variable, levels = c("Rainy Season\n\n", "Total Weekly Precipitation\n\n", "Heavy Rain\n\n")),
         level = as.character(level),
         level = factor(level, levels = c("Below Median", "Above Median", "No Heavy Rain", "Heavy Rain", "Not rainy season", "Rainy season")))

#--------------------------------------------
# Plot categorical ppt results
#--------------------------------------------

color_pal = c("Above Median" = "#0d5c70",
              "Below Median" = "#93c3cf",
              "Heavy Rain" = "#20363b",
              "No Heavy Rain" = "#95b3ba",
              "Rainy season" = "#287c8f",
              "Not rainy season" = "#b6d8e0")

shape_pal = c("Above Median" = 16,
              "Below Median" = 15,
              "Heavy Rain" = 16,
              "No Heavy Rain" = 15,
              "Rainy season" = 16,
              "Not rainy season" = 15)

tmle_ppt_plt = 
  ggplot(tmle_pooled, aes(x = contrast, y = psi)) +
  facet_wrap(~Variable, scales = "free_x") + 
  geom_hline(yintercept = 1, col = "#A4A5A5", alpha = 0.6) +
  geom_point(aes(col = level, shape = level), position = position_dodge(width = 0.5), size = 2.5) +
  geom_linerange(aes(ymin = CI1, ymax = CI2, col = level), position = position_dodge(width = 0.5), lwd = 1) +
  scale_y_continuous(trans = "log", limits = c(0.2, 4),
                     breaks = c(0.25, 0.5, 1, 2, 4)) +
  scale_x_discrete(limits = rev) +
  scale_shape_manual(name = "Level", values = shape_pal, limits = rev)+
  scale_color_manual(name = "Level", values = color_pal, limits = rev) +
  xlab("") + 
  ylab("Prevalence Ratio with 95% CI") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill="white"), 
        strip.text.x = element_text(size = 10, face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_flip()

tmle_ppt_plt

ggsave(glue("{fig_dir}Figure3-ppt_tmle_plot.png"), tmle_ppt_plt, height = 5, width = 10)
