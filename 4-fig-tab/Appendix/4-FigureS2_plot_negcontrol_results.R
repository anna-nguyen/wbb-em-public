################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Plot negative control results
################################################################################################

library(here)
source(here::here("0-config.R"))

#--------------------------------------------
# Load in results
#--------------------------------------------
tmle_diarrhea_pooled_negcontrol = readRDS(paste0(res_dir, "categorical-pooled/tmle_diarrhea_negcontrol.RDS")) %>% 
  prepare_plot_df(estimate_type = "RR") %>% 
  filter(contrast != "Any Water vs. Control") %>% 
  mutate(contrast = str_remove(contrast, " vs. Control"),
         contrast = ifelse(contrast == "Pooled", "Any WASH Intervention", contrast),
         contrast = factor(contrast, levels = c("Any WASH Intervention", "Combined WSH", "Water", "Sanitation", "Handwashing")), 
         Variable = case_when(str_detect(risk_factor, "heavyrain") ~ "Heavy Rain\n\n",
                              str_detect(risk_factor, "ppt") ~ "Total Weekly Precipitation\n\n",
                              str_detect(risk_factor, "season") ~ "Rainy Season\n\n",
                              str_detect(risk_factor, "avg") ~ "Average\n\n",
                              str_detect(risk_factor, "min") ~ "Minimum\n\n",
                              str_detect(risk_factor, "max") ~ "Maximum\n\n"), 
         Variable = factor(Variable, levels = c("Rainy Season\n\n", "Total Weekly Precipitation\n\n", "Heavy Rain\n\n", "Minimum\n\n", "Average\n\n", "Maximum\n\n")),
         level = as.character(level),
         level = factor(level, levels = c("Below Median", "Above Median", "No Heavy Rain", "Heavy Rain", "Not rainy season", "Rainy season")))

# Negative Control Tables --------------------------------------------------------------------
tmle_diarrhea_pooled_negcontrol %>% 
  mutate(Variable = str_remove(Variable, "\n\n")) %>% 
  select(Variable, level, PR = psi, CI_lb = CI1, CI_ub = CI2) %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/ExcelS7_negative_control_estimates.csv")), row.names = F)


# Temperature Plots --------------------------------------------------------------------
color_pal = c("Above Median" = "#c23321",
              "Below Median" = "#e6a294")

shape_pal = c("Above Median" = 16,
              "Below Median" = 15)

temp_negcontrol_plt = 
  ggplot(tmle_diarrhea_pooled_negcontrol %>% filter(str_detect(risk_factor, "temp")), 
         aes(x = Variable, y = psi)) +
  geom_hline(yintercept = 1, col = "#A4A5A5", alpha = 0.6) +
  geom_point(aes(col = level, shape = level), position = position_dodge(width = 0.5), size = 2.5) +
  geom_linerange(aes(ymin = CI1, ymax = CI2, col = level), position = position_dodge(width = 0.5), lwd = 1) +
  scale_y_continuous(trans = "log", limits = c(0.5, 2),
                     breaks = c(0.5, 1, 2)) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(name = "Level", values = color_pal, limits = rev) +
  scale_shape_manual(name = "Level", values = shape_pal, limits = rev)+
  xlab("") + 
  ylab("Prevalence Ratio with 95% CI") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_flip() +
  ggtitle("Temperature")

# PPT plots --------------------------------------------------------------------
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

ppt_negcontrol_plt = 
  ggplot(tmle_diarrhea_pooled_negcontrol %>% 
           filter(str_detect(risk_factor, "ppt") | str_detect(risk_factor, "rain") | str_detect(risk_factor, "season")) %>% 
           mutate(level = factor(level, 
                                 levels = c("No Heavy Rain", "Heavy Rain", "Below Median", "Above Median", "Not rainy season", "Rainy season"))), 
         aes(x = Variable, y = psi)) +
  geom_hline(yintercept = 1, col = "#A4A5A5", alpha = 0.6) +
  geom_point(aes(col = level, shape = level), position = position_dodge(width = 0.5), size = 2.5) +
  geom_linerange(aes(ymin = CI1, ymax = CI2, col = level), position = position_dodge(width = 0.5), lwd = 1) +
  scale_y_continuous(trans = "log", limits = c(0.5, 2),
                     breaks = c(0.5, 1, 2)) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(name = "Level", values = color_pal, limits = rev) +
  scale_shape_manual(name = "Level", values = shape_pal, limits = rev)+
  xlab("") + 
  ylab("Prevalence Ratio with 95% CI") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_flip() +
  ggtitle("Precipitation")


neg_control_plots_with_legend = grid.arrange(ppt_negcontrol_plt, temp_negcontrol_plt, nrow = 1, widths = c(4.75, 4))

ggsave(glue("{fig_dir}FigureS2-neg_control_plots_with_legend.png"), neg_control_plots_with_legend, width = 12, height = 4)

neg_control_plots_no_legend = 
  grid.arrange(ppt_negcontrol_plt + theme(legend.position = "none"), 
               temp_negcontrol_plt + theme(legend.position = "none"), 
               nrow = 1,
               widths = c(4.75, 4))

ggsave(glue("{fig_dir}FigureS2-neg_control_plots_no_legend.png"), neg_control_plots_no_legend, width = 12, height = 4)
