################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Plot diarrhea prevalence as a function of each type of temperature measure
# Plot estimates for temperature measures
################################################################################################

library(here)
source(here::here("0-config.R"))

tr_color_pal = c("#88A4BE", "#E8988A")
tr_linetypes = c("solid", "twodash")

temp_cutoffs = readRDS(paste0(local_box_data_path, "fldas_temp_cutoffs")) 

##########################################################
# Plot continuous temperature models 
##########################################################

format_plt_tbl = function(risk_factor){
  plt_df = readRDS(paste0(res_dir,"continuous-pooled/", glue::glue("pred_diarrhea_diar7d_{risk_factor}_adjusted.RDS")))
  plt_df[[1]] %>% 
    mutate(variable = risk_factor, 
           tr = ifelse(tr == "Control", "Control", "Any WASH Intervention")) %>% 
    arrange(tr, !!sym(risk_factor)) %>% 
    select(variable, tr, value = !!sym(risk_factor), fit, lwrS, uprS) %>% 
    distinct()
  }

lapply(c("temp_min_1weeklag", "temp_max_1weeklag", "temp_avg_1weeklag"), format_plt_tbl) %>%
  bind_rows() %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/ExcelS5_diar_prev_by_temp.csv")), row.names = T)

plot_continuous_temperatures = function(risk_factor, label, title) {
  plt_df = readRDS(paste0(res_dir,"continuous-pooled/", glue::glue("pred_diarrhea_diar7d_{risk_factor}_adjusted.RDS")))
  
  med_val = case_when(risk_factor == "temp_min_1weeklag" ~ "absmintemp_7_days_median_cutoff", 
                      risk_factor == "temp_max_1weeklag" ~ "absmaxtemp_7_days_median_cutoff",
                      risk_factor == "temp_avg_1weeklag" ~ "avgtemp_7_days_median_cutoff") 
  
  plt = ggplot(data=plt_df[[1]] %>% 
                 mutate(tr = ifelse(tr == "Pooled", "Any WASH Intervention", "Control"),
                        uprS = ifelse(uprS > 10, 10, uprS)),
               aes_string(x=risk_factor,
                          y="fit",
                          group="tr",
                          color="tr",
                          fill="tr", 
                          linetype = "tr")) +
    geom_ribbon(aes(ymin=lwrS,ymax=uprS),alpha=0.3,color=NA) +
    geom_line(lwd=1,alpha=1) +
    scale_color_manual(name = "Intervention Group", values = tr_color_pal) + 
    scale_fill_manual(name = "Intervention Group", values = tr_color_pal) + 
    scale_linetype_manual(name = "Intervention Group", values = tr_linetypes) + 
    xlab("Weekly Total Precipitation, mm (1 week lag)") +
    ylab("Diarrhea Prevalence (%)") + 
    ggtitle(title) + 
    theme_minimal()  +
    theme(legend.position="bottom",
          #legend.title = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title.position = "plot",
          plot.margin=unit(c(5.5,5.5,0,5.5),"pt"))
  
  
  plot_density = 
    ggplot(plt_df[[1]] %>% mutate(tr = ifelse(tr == "Pooled", "Any WASH Intervention", "Control")), 
           aes(x = !!sym(risk_factor), color=tr, fill=tr, linetype = tr)) +
    geom_density(alpha=0.3) +
    geom_vline(xintercept = temp_cutoffs[[med_val]] - 273.15, color = "#666666", linetype = "dashed", size = 0.5) + 
    xlab(label) + 
    ylab("\n") + 
    theme_minimal() +
    scale_color_manual(name = "Intervention Group", values = tr_color_pal) + 
    scale_fill_manual(name = "Intervention Group", values = tr_color_pal) + 
    scale_linetype_manual(name = "Intervention Group", values = tr_linetypes) + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.y=element_blank(),
      plot.margin=unit(c(0,5.5,5.5,5.5),"pt")
    )
  
  grid.arrange(plt, plot_density, heights = c(4,1))
}

min_temp_plot = plot_continuous_temperatures(risk_factor = "temp_min_1weeklag", 
                                             label = "Minimum weekly temperature (1 week lag)", 
                                             title = "Minimum Temperature")

max_temp_plot = plot_continuous_temperatures(risk_factor = "temp_max_1weeklag", 
                                             label = "Maximum weekly temperature (1 week lag)", 
                                             title = "Maximum Temperature")

avg_temp_plot = plot_continuous_temperatures(risk_factor = "temp_avg_1weeklag", 
                                             label = "Average weekly temperature (1 week lag)", 
                                             title = "Average Temperature")

continuous_temp_plots = grid.arrange(min_temp_plot, avg_temp_plot, max_temp_plot, nrow = 1)

##########################################################
# Plot categorical temperature models 
##########################################################

tmle_diarrhea <- readRDS(paste0(res_dir,"categorical/tmle_diarrhea_adjusted.RDS")) %>% 
  filter(risk_factor == "season" | str_detect(risk_factor, "1week"))

tmle_diarrhea_pooled <- readRDS(paste0(res_dir,"categorical-pooled/tmle_diarrhea_adjusted.RDS")) %>% 
  filter(risk_factor == "season" | str_detect(risk_factor, "1week"))

tmle_pooled = tmle_diarrhea %>% 
  bind_rows(tmle_diarrhea_pooled) %>% 
  filter(str_detect(risk_factor, "temp"), !str_detect(risk_factor, "heatwave")) %>% 
  prepare_plot_df(estimate_type = "RR") %>% 
  mutate(contrast = str_remove(contrast, " vs. Control"),
         contrast = ifelse(contrast == "Pooled", "Any WASH Intervention", contrast),
         contrast = factor(contrast, levels = c("Any WASH Intervention", "Combined WSH", "Water", "Sanitation", "Handwashing")), 
         Variable = case_when(str_detect(risk_factor, "avg") ~ "Average\n\n",
                              str_detect(risk_factor, "min") ~ "Minimum\n\n",
                              str_detect(risk_factor, "max") ~ "Maximum\n\n"), 
         Variable = factor(Variable, levels = c("Minimum\n\n", "Average\n\n", "Maximum\n\n")),
         level = as.character(level),
         level = case_when(Variable == "Average\n\n" ~ paste0(level, ""),
                           Variable == "Minimum\n\n" ~ paste0(level, " min"),
                           Variable == "Maximum\n\n" ~ paste0(level, " max"), 
                           T ~ level),
         level = factor(level, levels = c("Below Median max", "Above Median max", 
                                          "Below Median min", "Above Median min",
                                          "Below Median", "Above Median")))

color_pal = c("Above Median max" = "#751C09",
              "Below Median max" = "#B1503B",
              "Above Median" = "#AF3419",
              "Below Median" = "#D58978",
              "Above Median min" = "#DF4B2A",
              "Below Median min" = "#EA8E79")
              

shape_pal = c("Above Median max" = 16,
              "Below Median max" = 15,
              "Above Median" = 16,
              "Below Median" = 15,
              "Above Median min" = 16,
              "Below Median min" = 15)

tmle_temp_plt = 
  ggplot(tmle_pooled, 
         aes(x = contrast, y = psi)) +
    facet_wrap(~Variable, scales = "free_x") + 
    geom_hline(yintercept = 1, col = "#A4A5A5", alpha = 0.6) +
    geom_point(aes(col = level, shape = level), position = position_dodge(width = 0.5), size = 2.5) +
    geom_linerange(aes(ymin = CI1, ymax = CI2, col = level), position = position_dodge(width = 0.5), lwd = 1) +
    scale_y_continuous(trans = "log", limits = c(0.2, 4),
                       breaks = c(0.25, 0.5, 1, 2, 4)) +
    scale_x_discrete(limits = rev) +
    scale_shape_manual(name = "Level", values = shape_pal, limits = rev) +
    scale_color_manual(name = "Level", values = color_pal, limits = rev) +
    xlab("") + 
    ylab("Prevalence Ratio with 95% CI") +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(family = "", size = 12),
          strip.background = element_rect(fill="white"), 
          strip.text.x = element_text(size = 10, face = "bold"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    coord_flip()


combined_temp_plot = grid.arrange(continuous_temp_plots, tmle_temp_plt, nrow = 2, heights = c(3.5, 5))
ggsave(glue("{fig_dir}Figure4-combined_temp_plots.png"), combined_temp_plot, height = 8, width = 12)

