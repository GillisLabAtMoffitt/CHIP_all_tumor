# import packages
library(tidyverse)
library(lubridate)

########################################################################################## I ### load data----
Global_data <- read_rds(paste0(here::here(), "/Global_data.rds"))


########################################################################################## II ### create variables----
patient_data <- Global_data %>% 
  mutate(age_at_diagnosis = coalesce(age_at_diagnosis.x, age_at_diagnosis.y)) %>% 
  select(-c(age_at_diagnosis.x, age_at_diagnosis.y)) %>% 
  # OS variables
  mutate(os_event =  case_when(
    vital_status == "Dead"                    ~ 1,
    vital_status != "Dead"                    ~ 0,
    is.na(vital_status)                       ~ 0
  )) %>% 
  mutate(age_at_germline = interval(start= dob, end= date_of_specimen_collection)/                      
           duration(n=1, unit="years")) %>% 
  mutate(os_time = coalesce(age_at_death, age_at_last_contact, last_age),
         os_time = os_time - age_at_germline) %>% 
  # Germline var
  mutate(germlineBFdrugs = case_when(
    age_at_germline > age_at_med_start1             ~ "No",
    age_at_germline <= age_at_med_start1            ~ "OK",
    is.na(age_at_med_start1)                        ~ "OK"
  )) %>% 
  mutate(germlineBFsct = case_when(
    age_at_germline > age_at_transplant1            ~ "No",
    age_at_germline <= age_at_transplant1           ~ "OK",
    is.na(age_at_transplant1)                       ~ "OK"
  )) %>% 
  mutate(germlineBFrad = case_when(
    age_at_germline <= age_at_radiation_start1      ~ "OK",
    age_at_germline > age_at_radiation_start1       ~ "No",
    is.na(age_at_radiation_start1)                  ~ "OK"
  )) %>% 
  mutate(drugs_ever = case_when(
    drugs_ever == "Yes"                             ~ "Yes",
    is.na(drugs_ever)                               ~ "No"
  )) %>% 
  mutate(radiation_ever = case_when(
    radiation_ever == "Yes"                         ~ "Yes",
    is.na(radiation_ever)                           ~ "No"
  )) %>% 
  mutate(sct_ever = case_when(
    sct_ever == "Yes"                               ~ "Yes",
    is.na(sct_ever)                                 ~ "No"
  )) %>% 
  mutate(across(c(ends_with("_ever")), ~factor(., levels = c("No", "Yes"))))

write_rds(patient_data, "patient_data.rds")


germline_data <- patient_data %>% 
  filter(!is.na(Germline_SLID))
write_rds(germline_data, "germline_data.rds")


# END Create var
