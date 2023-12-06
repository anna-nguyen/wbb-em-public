################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Plot sensitivity analysis comparing heavy rain cutoff for 80th vs 90th percentile
################################################################################################

library(here)
source(here::here("0-config.R"))

cutoffs = readRDS(paste0(box_data_path, "ppt_cutoffs.RDS")) 
cutoff_80 = glue::glue("80th Percentile ({round(cutoffs$heavy_rain_cutoff, 1)} mm)")
cutoff_90 = glue::glue("90th Percentile ({round(cutoffs$heavy_rain_cutoff_90th, 1)} mm)")

tmle_diarrhea_pooled <- readRDS(paste0(res_dir,"categorical-pooled/tmle_diarrhea_adjusted.RDS")) %>% 
  filter(str_detect(risk_factor, "heavy"), str_detect(risk_factor, "1week")) %>%  
  filter(parameter == "RR") %>%
  mutate(cutoff = ifelse(risk_factor == "heavyrain_1weeklag", cutoff_80, cutoff_90), 
         level = ifelse(level == "yes", "Heavy Rain", "No Heavy Rain"), 
         level = factor(level, levels = c("No Heavy Rain", "Heavy Rain"))) 

tmle_diarrhea_pooled %>% 
  mutate(Variable = "Heavy Rain") %>% 
  select(Cutoff = cutoff, Variable, Level = level, PR = psi, CI_lb = CI1, CI_ub = CI2) %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/ExcelS10_heavyrain_cutoffs.csv")), row.names = F)

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

plt_heavy_rain_defs = 
  ggplot(tmle_diarrhea_pooled, aes(x = cutoff, y = psi)) +
  geom_hline(yintercept = 1, col = "#A4A5A5", alpha = 0.6) +
  geom_point(aes(col = level, shape = level), position = position_dodge(width = 0.5), size = 2.5) +
  geom_linerange(aes(ymin = CI1, ymax = CI2, col = level), position = position_dodge(width = 0.5), lwd = 1) +
  scale_y_continuous(trans = "log", limits = c(0.2, 2),
                     breaks = c(0.25, 0.5, 1, 2)) +
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
  coord_flip() + 
  ggtitle("PRs under Different Heavy Rain Cutoffs")

ggsave(here::here("figures", "FigureS5-sa_heavy_rain_definitions.png"), 
       plt_heavy_rain_defs, height = 4, width = 6)

