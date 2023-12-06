#######################################################
# Project: Wash Benefits Bangladesh

# Description: Configuration file for all scripts
#######################################################

#renv::restore()

run_on_cluster = F
#--------------------------------------------
# Load in libraries
#--------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(boxr)
library(here)
library(metafor)
library(washb)
library(gamm4)
library(glue)
library(data.table)

library(devtools)
library(ggplot2)
library(lubridate)
library(forestplot)
library(grid)
library(gridExtra)
library(cowplot)
library(lemon)

library(foreach)
library(parallel)
library(doParallel)
library(stringr)

library(tictoc)

#--------------------------------------------
# Set up Box authentication
#--------------------------------------------
# path to box on each person's local
if(Sys.getenv("HOME") == "/Users/arushapatil"){
  local_root_path = "/Users/arushapatil/Library/CloudStorage/Box-Box/"
} else if(Sys.getenv("HOME") == "/Users/annanguyen") {
  local_root_path = "/Users/annanguyen/Box Sync/"        
} else if(Sys.getenv("HOME") == "/Users/nikkibarratt") {
  local_root_path = "/Users/nikkibarratt/Library/CloudStorage/Box-Box/"        
} else if(Sys.getenv("HOME") == "/Users/whutson9"){
  local_root_path = "/Users/whutson9/Library/CloudStorage/Box-Box/"
} else {
  local_root_path = ""
}

local_box_path = "WBB-mapping-Stanford/"
box_data_path = paste0(local_root_path, local_box_path, "wbb-em-data/")
local_box_data_path = paste0(local_root_path, local_box_path, "data/")
#--------------------------------------------
# Set directory paths
#--------------------------------------------

data_dir = paste0(here::here(), "/data/")
res_dir = paste0(here::here(), "/results/")
tables_dir = paste0(here::here(), "/tables/")
fig_dir = paste0(here::here(), "/figures/")

#--------------------------------------------
# Load in WB data for Bangladesh
#--------------------------------------------
cleaned_d_diarr_filename = "wbb_mapping_bangladesh_data_diarr.RDS"
merged_d_diarr_filename = "washb-bangladesh-merged-diarr.RDS"

cleaned_d_diarr_filepath = glue("{box_data_path}{cleaned_d_diarr_filename}")
merged_d_diarr_filepath = glue("{box_data_path}{merged_d_diarr_filename}")

if (run_on_cluster) {
  diarr_df_path = here::here("data", "washb-bangladesh-merged-diarr_offset.RDS")
} else {
  diarr_df_path = paste0(box_data_path, merged_d_diarr_filename)
}

#--------------------------------------------
# Load utility functions
#--------------------------------------------

util_functions = list.files(paste0(here::here(), "/0-Utils/"), pattern = "*.R")
for (util in util_functions) {
  source(paste0(here::here(), "/0-Utils/", util))
} 

#--------------------------------------------
# Create list for contrasts
#--------------------------------------------

contrast_list = list(
  c("Control", "Water"),
  c("Control", "Sanitation"),
  c("Control", "Handwashing"),
  c("Control", "Combined WSH")
)

pooled_contrast_list = list(c("Control", "Pooled"))


