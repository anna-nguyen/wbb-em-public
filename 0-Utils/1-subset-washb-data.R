################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Function to subset all diarrhea measurements from the WASH trials to the ones
# that are relevant to our study cohort
################################################################################################

subset_washb_diarr_df = function(df) {
  df %>%
    mutate(date_baseline_svy = as.Date(strptime(date_baseline_svy, format = "%d%b%Y"))) %>% # Reformat dates
    filter(!is.na(diar7d), # Remove fd
           cohort != "EE diarrhea", # Remove observations from the environmental enteropathy cohort
           sibnewbirth == 0, # Remove children who were born after the index children
           gt36mos == 0, # Restrict to only children who were older than 36 months at enrollment
           date > date_baseline_svy) # Restrict to post-intervention measurements
}
