################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Obtain model predictions over observed levels of each risk factor by arm
################################################################################################

rm(list = ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(future)

run_on_cluster = F
set.seed(0)

# Generate categorical results by intervention type
generate_results_categorical(outcome = "diarrhea", adjusted = T)

# Generate categorical results by pooled intervention
generate_results_categorical_pooled(outcome = "diarrhea", adjusted = T)

# Generate negative control results by pooled intervention
generate_results_categorical_pooled_neg_control(outcome = "diarrhea")
