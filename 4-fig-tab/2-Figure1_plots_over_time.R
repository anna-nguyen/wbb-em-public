################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Plot diarrhea prevalence, temperature, and preciptiation over time
################################################################################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(scales)
tr_color_pal = c("#88A4BE", "#E8988A")
tr_linetypes = c("solid", "twodash")

# Diarrhea ----
d_diarrhea = readRDS(diarr_df_path) %>% 
  filter(tr != "Nutrition") %>% 
  mutate(tr = ifelse(tr == "Control", "Control", "Any WASH Intervention"),
         tr = as.factor(tr), 
         diar7d = as.numeric(diar7d) - 1) %>% 
  filter(date >= as.Date("2013-09-01"))

# PPT data ----
all_ppt_data = readRDS(paste0(local_box_data_path, "all_ppt_data_with_pixelID.RDS")) 
daily_ppt = all_ppt_data %>% 
  group_by(date, pixelID) %>% 
  summarise(total_ppt = sum(ppt)) %>% 
  group_by(date) %>% 
  summarize(total_ppt = mean(total_ppt))

season_dates = read.csv(paste0(box_data_path, "season_definitions.csv")) %>% 
  mutate(start = as.Date(start), end = as.Date(end))

# Temp data ----
all_daily_fldas_data = readRDS(paste0(local_box_data_path, "daily_temperatures_fldas.RDS"))
daily_temp = all_daily_fldas_data %>% 
  group_by(date, cells) %>% 
  summarise(avg_temp = mean(temp, na.rm = T)) %>% 
  group_by(date) %>% 
  summarise(avg_temp = mean(avg_temp, na.rm = T) - 273.15) 

generate_prev_over_time = function(df, outcome_var, title_str, include_rug_plots = T, include_legend = F) { 
  df_monthly = df
  day(df_monthly$date) = 1
  
  prev_monthly = df_monthly %>% 
    group_by(date, tr) %>% 
    summarise(data.frame(washb_mean(.data[["diar7d"]], id = .data$clusterid, print = F))) %>%
    mutate(Mean = Mean*100) %>% 
    select(date, tr, Mean)
  
  prev_monthly = 
    expand.grid(date = seq.Date(from = min(prev_monthly$date), to = max(prev_monthly$date), by = "month"), 
                tr = unique(prev_monthly$tr)) %>%
    left_join(prev_monthly, by = c("date", "tr"))
  
  date_range = range(prev_monthly$date)
  date_range[1] = date_range[1] - 1
  date_range[2] = date_range[2] + 1
  
  prev_over_time = 
    ggplot(prev_monthly, aes(x = date, y = Mean, group = tr, color = tr, fill = tr, linetype = tr)) + 
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[1]), xmax=min(date_range[2], season_dates$end[1]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[2]), xmax=min(date_range[2], season_dates$end[2]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") + 
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[3]), xmax=min(date_range[2], season_dates$end[3]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[4]), xmax=min(date_range[2], season_dates$end[4]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[5]), xmax=min(date_range[2], season_dates$end[5]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_line(linewidth = 1) + 
    theme_minimal() + 
    xlab("Date") + 
    ylab(" ") +
    ggtitle("A) Diarrhea Prevalence (%)") +    
    scale_x_date(date_labels = "%b %Y", 
                 limits = date_range, 
                 breaks = as.Date(c("2013-09-01", "2014-01-01", "2014-05-01", "2014-09-01", "2015-01-01", "2015-05-01", "2015-09-01"))) +
    scale_y_continuous(breaks = seq(2, 14, 2)) + 
    scale_color_manual(name = "Intervention Group", values = tr_color_pal) + 
    scale_fill_manual(name = "Intervention Group", values = tr_color_pal) + 
    scale_linetype_manual(name = "Intervention Group", values = tr_linetypes) + 
    #theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot") 
  
  daily_ppt_plt = 
    ggplot(daily_ppt %>% filter(date >= date_range[1]), aes(x = date, y = total_ppt)) + 
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[1]), xmax=min(date_range[2], season_dates$end[1]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[2]), xmax=min(date_range[2], season_dates$end[2]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") + 
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[3]), xmax=min(date_range[2], season_dates$end[3]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[4]), xmax=min(date_range[2], season_dates$end[4]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[5]), xmax=min(date_range[2], season_dates$end[5]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_line(color = "#89CFF0") + 
    theme_minimal() + 
    xlab("Date") + 
    ylab(" ")+ 
    ggtitle("B) Total Precipitation (mm)") +    
    scale_x_date(date_labels = "%b %Y", 
                 limits = date_range, 
                 breaks = as.Date(c("2013-09-01", "2014-01-01", "2014-05-01", "2014-09-01", "2015-01-01", "2015-05-01", "2015-09-01"))) +
    theme(plot.title.position = "plot",
          axis.title.x = element_blank())
  
  write.csv(daily_ppt %>% filter(date >= date_range[1]), paste0(tables_dir, glue::glue("Appendix/ExcelS2_daily_ppt.csv")))
  
  daily_temp_plt =
    ggplot(daily_temp %>% filter(date >= date_range[1]), aes(x = date, y = avg_temp)) +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[1]), xmax=min(date_range[2], season_dates$end[1]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[2]), xmax=min(date_range[2], season_dates$end[2]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[3]), xmax=min(date_range[2], season_dates$end[3]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[4]), xmax=min(date_range[2], season_dates$end[4]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    geom_rect(aes(xmin=max(date_range[1], season_dates$start[5]), xmax=min(date_range[2], season_dates$end[5]), ymin=-Inf, ymax=Inf), color = "#F5F5F5", fill = "#F5F5F5") +
    #geom_bar(stat = "identity", color = "#E8988A", fill = "#750000") +
    geom_line(color = "#CA3433") +
    theme_minimal() +
    xlab("Date") +
    ylab(" ") + 
    ggtitle("C) Average Temperature (°C)") +
    scale_x_date(date_labels = "%b %Y",
                 limits = date_range,
                 breaks = as.Date(c("2013-09-01", "2014-01-01", "2014-05-01", "2014-09-01", "2015-01-01", "2015-05-01", "2015-09-01"))) +
    theme(plot.title.position = "plot",
          axis.title.x = element_blank())
  
  write.csv(daily_temp %>% filter(date >= date_range[1]), paste0(tables_dir, glue::glue("Appendix/ExcelS3_daily_temp.csv")))

  if (include_legend) { 
    prev_over_time = prev_over_time + theme(legend.position = "bottom")
  } else {
    prev_over_time = prev_over_time + theme(legend.position = "none")
  }
  
  if (!include_rug_plots) { return (prev_over_time) }
  
  control_n = df_monthly %>% filter(tr == "Control") %>% group_by(date) %>% count()
  
  control_sample_size = 
    ggplot(control_n, aes(x = date, y = n)) + 
    geom_bar(stat = "identity", color = "#E8988A", fill = "#E8988A") + 
    theme_minimal() +  
    xlab("Date") + 
    ylab(" ") + 
    xlim(date_range) +
    scale_x_date(date_labels = "%b %Y",
                 #limits = date_range,
                 breaks = as.Date(c("2013-09-01", "2014-01-01", "2014-05-01", "2014-09-01", "2015-01-01", "2015-05-01", "2015-09-01")))+
    scale_y_continuous(breaks = seq(0, 1000, 100)) + 
    theme(axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0.375), "cm"))  
  
  intervention_n = df_monthly %>% filter(tr == "Any WASH Intervention") %>% group_by(date) %>% count() 
  
  intervention_sample_size = 
    ggplot(intervention_n, aes(x = date, y = n)) + 
    geom_bar(stat = "identity", color = "#88A4BE", fill = "#88A4BE") + 
    theme_minimal() +  
    xlab("Date") + 
    ylab(" ") + 
    xlim(date_range) +
    scale_x_date(date_labels = "%b %Y",
                 #limits = date_range,
                 breaks = as.Date(c("2013-09-01", "2014-01-01", "2014-05-01", "2014-09-01", "2015-01-01", "2015-05-01", "2015-09-01"))) +
    scale_y_continuous(breaks = seq(0, 1000, 200)) + 
    theme(axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0.375), "cm"))  
  
  prev_monthly %>% 
    left_join(intervention_n %>% rename("n_intervention" = n), by = "date") %>% 
    left_join(control_n %>% rename("n_control" = n), by = "date") %>% 
    mutate(n_intervention = ifelse(is.na(n_intervention), 0, n_intervention),
           n_control = ifelse(is.na(n_control), 0, n_control)) %>% 
    write.csv(paste0(tables_dir, glue::glue("Appendix/ExcelS1_prev_over_time.csv")))
  
  grid.arrange(prev_over_time, control_sample_size, intervention_sample_size, daily_ppt_plt, daily_temp_plt,   
               ncol = 1, heights = c(4, .3, .3, 2, 2))
}

plot_over_time_no_legend = generate_prev_over_time(df = d_diarrhea, outcome_var = "diar7d", title_str = "", include_legend = F)
ggsave(glue("{fig_dir}Figure1-plot_over_time_no_legend.png"), plot_over_time_no_legend, height = 7, width = 7)

plot_over_time_legend = generate_prev_over_time(df = d_diarrhea, outcome_var = "diar7d", title_str = "", include_legend = T)
ggsave(glue("{fig_dir}Figure1-plot_over_time_with_legend.png"), plot_over_time_legend, height = 7.5, width = 7)


