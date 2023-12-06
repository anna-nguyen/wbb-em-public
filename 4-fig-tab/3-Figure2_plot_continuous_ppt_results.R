################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Plot diarrhea prevalence as a function of total precipitation
################################################################################################

source(here::here("0-config.R"))

pred_ppt_model = readRDS(paste0(res_dir,"continuous-pooled/", "pred_diarrhea_diar7d_ppt_sum_1weeklag_adjusted.RDS"))
pred_ppt_model_df = pred_ppt_model[[1]] %>% 
  mutate(tr = ifelse(tr == "Pooled", "Any WASH Intervention", "Control"))
tr_color_pal = c("#88A4BE", "#E8988A")
tr_linetypes = c("solid", "twodash")
risk_factor = "ppt_sum_1weeklag"

pred_ppt_model_df

pred_ppt_model_df %>% 
  select(tr, ppt_sum_1weeklag, fit, lwrS, uprS) %>% 
  arrange(tr, ppt_sum_1weeklag) %>% 
  distinct() %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/ExcelS4_diar_prev_by_ppt.csv")))

model_pred_plt = 
  ggplot(pred_ppt_model_df, aes_string(x=risk_factor, y="fit", group="tr", color="tr", fill="tr", linetype = "tr")) +
  geom_ribbon(aes(ymin=lwrS,ymax=uprS),alpha=0.3,color=NA) +
  geom_line(lwd=1,alpha=1) +
  geom_vline(aes(xintercept = quantile(!!sym(risk_factor), 0.1) %>% as.numeric()), color = "#666666", linetype = "dotted") + 
  geom_vline(aes(xintercept = quantile(!!sym(risk_factor), 0.9) %>% as.numeric()), color = "#666666", linetype = "dotted") + 
  scale_color_manual(name = "Intervention Group", values = tr_color_pal) + 
  scale_fill_manual(name = "Intervention Group", values = tr_color_pal) + 
  scale_linetype_manual(name = "Intervention Group", values = tr_linetypes) + 
  scale_x_continuous(breaks = seq(0, 300, by = 25)) +
  xlab("Weekly Total Precipitation, mm (1 week lag)") +
  ylab("Diarrhea Prevalence (%)") + 
  theme_minimal()  +
  theme(legend.position="bottom",
        #legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(5.5,5.5,0,5.5),"pt"))

model_pred_plt

ppt_density_plt = 
  ggplot(pred_ppt_model_df, aes(x = !!sym(risk_factor), color=tr, fill=tr, linetype = tr)) +
  geom_density(alpha=0.3) +
  geom_vline(aes(xintercept = 13), color = "#666666", linetype = "dotted", linewidth = 0.5) + 
  xlab("Total Weekly Precipitation, mm (1 week lag)") + 
  ylab("\n") + 
  theme_minimal() +
  scale_color_manual(name = "Intervention Group", values = tr_color_pal) + 
  scale_fill_manual(name = "Intervention Group", values = tr_color_pal) + 
  scale_linetype_manual(name = "Intervention Group", values = tr_linetypes) + 
  scale_x_continuous(breaks = seq(0, 300, by = 25)) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y=element_blank(),
    plot.margin=unit(c(0,5.5,5.5,5.5),"pt")
  )

combined_ppt_plot <- grid.arrange(model_pred_plt, ppt_density_plt, heights = c(5,2))
ggsave(glue("{fig_dir}Figure2-continuous_ppt_pred.png"), combined_ppt_plot, height = 4, width = 5)



