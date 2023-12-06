################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Clean bangladesh main survey baseline, midline, & endline diarrhea data
################################################################################################
rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(rdrop2)

#--------------------------------------------
# set up Dropbox authentication
#--------------------------------------------
my_dropbox_token <- drop_auth(cache = F)

#--------------------------------------
# load and clean raw bangladesh diarrhea data
#--------------------------------------
## load main survey diarrhea data 
bd_main_diarr <- drop_read_csv("WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-diar.csv", dtoken = my_dropbox_token) %>%
  mutate(svydate = as.Date(svydate, format = "%d%b%Y"), 
         dob = as.Date(dob, format = "%d%b%Y"), 
         cohort = "main diarrhea") %>%
  # Remove all children less than 6 months of age due to issues with accuracy of caregiver diarrhea for this age group
  filter(agedays > 182)

bd_diarr <- bd_main_diarr %>%
  rename(aged = agedays, 
         agey = ageyrs) %>%
  mutate(diar7d = factor(diar7d))

#--------------------------------------
# load enrolment/baseline bangladesh data
#--------------------------------------
raw_washb_path_box = paste0(local_root_path, local_box_path, "raw-washb/")
bbase = read.csv(paste0(raw_washb_path_box, "washb-bangladesh-enrol.csv"))

bbase = bbase %>% 
  # mutate(dataid = as.character(dataid)) %>%
  rename(date_baseline_svy = svydate)

#--------------------------------------
# merge diarrhea data with all covariate data
#--------------------------------------
## merge baseline covariate data
nrow(bd_diarr)
bd_diarr = left_join(bd_diarr, bbase, by = c("dataid", "clusterid", "block"))
nrow(bd_diarr)

## get latown variable from sth dataset
bsthmain = read.csv(paste0(raw_washb_path_box, "sth.csv"))

sth_tr <- bsthmain %>%
  select(clusterid, tr) %>%
  unique() 

bd_diarr = bd_diarr %>%
  left_join(sth_tr, by = "clusterid") %>%
  mutate(tr_received = factor(ifelse(svy == 0, "Control", tr)),
         tr = factor(tr))


#--------------------------------------
# create new covariate variables
#--------------------------------------
## create indicator for improved sanitation at baseline & intervention (whether subject's HH had an intervention at the time of sample collection)
bd_diarr = bd_diarr %>% 
  mutate(implatrine = factor(ifelse(latown==1 & latseal==1,"Improved latrine","No improved latrine")),
         intervention = as.factor(ifelse(svy == 0 | tr == "Control", 0, 1)))

## create month, year, quarter variables 
bd_diarr = bd_diarr %>%
  mutate(date = as.Date(svydate,"%d%b%Y"),
         month = month(svydate),
         year = year(svydate),
         quarter = factor(case_when(
           month <= 3 ~ "Q1",
           month > 3 & month <= 6 ~ "Q2",
           month > 6 & month <= 9 ~ "Q3",
           month > 9 & month <= 12 ~ "Q4")),
         age_cat = as.factor(ifelse(aged <= 1.5*365.25, "<1.5yr", ifelse(aged > 1.5*365.25, "1.5-5yr", NA)))) 
#--------------------------------------
# merge in lat long
#--------------------------------------
gps = read.csv(paste0(raw_washb_path_box, "gps_27aug2013_corrected_temporary.csv"))

gps = gps %>% dplyr::select(c(dataid, qgpslong, qgpslat, clusterid, block)) 

bdata = left_join(bd_diarr, gps, by = c("dataid", "clusterid", "block")) %>%
  mutate(clusterid = as.factor(clusterid),
         dataid = as.factor(dataid))

#--------------------------------------
# Save clean data
#--------------------------------------
saveRDS(bdata, cleaned_d_diarr_filepath)
