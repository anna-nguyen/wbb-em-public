################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Plot relationship between temperature and precipitation for diarrhea measurements
################################################################################################

rm(list = ls())
# ---------------------------------------------------------------
# Configure directories, load libraries and base functions
# ---------------------------------------------------------------
source(paste0(here::here(), "/0-config.R"))
run_on_cluster = F
# ---------------------------------------------------------------
# Load diarrhea data from Box
# ---------------------------------------------------------------
if (run_on_cluster) {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr_offset.RDS")
} else {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr.RDS")
}

d_diarrhea = readRDS(diarr_df_path) %>% 
  prepare_rf_variables()

d_diarrhea = d_diarrhea %>% 
  filter(!is.na(temp_avg_1weeklag), !is.na(ppt_sum_1weeklag))

# Save data
d_diarrhea %>% 
  select("Weekly average temperature" = temp_avg_1weeklag, "Weekly total precipitation" = ppt_sum_1weeklag) %>% 
  write.csv(paste0(tables_dir, glue::glue("Appendix/ExcelS6_cor_temp_ppt.csv")), row.names = F)

temp_ppt_plt = ggplot(d_diarrhea, aes(x = temp_avg_1weeklag, y = ppt_sum_1weeklag)) +
  geom_point(alpha = 0.5, size = 0.5) + 
  theme_light() + 
  xlab("Weekly Average Temperature (C)") +
  ylab("Weekly Total Precipitation (mm)") + 
  ggtitle("Temperature vs. precipitation, 1 week lag")

ggsave(paste0(fig_dir, "FigureS1-ppt_vs_temp.png"), temp_ppt_plt, height = 3.5, width = 5)
