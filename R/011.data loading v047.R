# import packages
library(tidyverse)
library(data.table)
library(lubridate)
# library(gtsummary)


########################################################################################## I ### load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                  "CH all tumor types")

Germline <- read_csv(paste0(path, "/raw data/Yi-Han/sample list with cancer type.csv")) %>% 
  select(avatar_key, Germline_SLID, disease_type_conformed)
sample_data_v4_7_dates <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "CDSC-AvatarMasterList_SDR-2 ",
                    na = "NULL") %>% 
  janitor::clean_names() %>% 
  select(orien_avatar_patient_id, mrn, dob,
         date_of_diagnosis, age_at_diagnosis, date_of_specimen_collection,
         dna_sequencing_library_id) %>% 
  mutate(mrn = as.character(mrn))

Germline <- left_join(Germline, sample_data_v4_7_dates,
                         by= c("avatar_key" = "orien_avatar_patient_id", 
                               "Germline_SLID" = "dna_sequencing_library_id"))
Demographics <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "20220504_MCC_PatientMaster_V4") %>% 
  janitor::clean_names() %>% 
  select(avatar_key, sex, race, ethnicity)

Vitals <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "20220504_MCC_VitalStatus_V4") %>% 
  janitor::clean_names() %>% 
  select(avatar_key, vital_status, age_at_last_contact, age_at_death)

Diagnosis_v4_7 <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "20220504_MCC_Diagnosis_V4 ") %>% 
  janitor::clean_names() %>% 
  select(avatar_key, age_at_diagnosis, primary_diagnosis_site_code, primary_diagnosis_site, 
         laterality, histology_code, histology, clin_t_stage, clin_group_stage,
         other_staging_system, other_staging_value)

Medication_v4_7 <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "20220504_MCC_Medications_V4") %>% 
  janitor::clean_names() %>% 
  select("avatar_key", "medication", "med_line_regimen", "age_at_med_start",
         "age_at_med_stop")

Radiation <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "20220504_MCC_Radiation_V4") %>% 
  janitor::clean_names() %>% 
  select(avatar_key, age_at_radiation_start, age_at_radiation_stop, rad_site, 
         rad_surgery_sequence, rad_dose, rad_fractions, rad_reason_none_given)
  
SCT <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "20220504_MCC_StemCellTransplant") %>% 
  janitor::clean_names() %>% 


########################################################################################## II ### data cleaning----
# Diagnosis
Diagnosis_v4_7 <- Diagnosis_v4_7 %>% 
  arrange(avatar_key, age_at_diagnosis) %>% 
  distinct(avatar_key, .keep_all = TRUE)

# Vitals
Vitals <- Vitals %>% 
mutate(across(.cols = c(starts_with("age_at")), ~as.numeric(.)))

# Drugs
medication_v4_7 <- Medication_v4_7 %>% 
  mutate(across(everything(), .fns = ~ na_if(., "Unknown/Not Applicable"))) %>% 
  mutate(across(.cols = c(age_at_med_start, age_at_med_stop), ~as.numeric(.))) %>% 
  filter(!is.na(medication)) %>% 
  arrange(age_at_med_start, age_at_med_stop)
  
Medication_v4_7 <- medication_v4_7 %>% 
  group_by(avatar_key, age_at_med_start) %>% 
  summarise_at(vars(medication, age_at_med_stop), paste, collapse = ", ") %>%
  group_by(avatar_key) %>% 
  summarise_at(vars(age_at_med_start, medication, age_at_med_stop), paste, collapse = ";") %>%
  mutate(regimen_count = sapply(strsplit(medication, ";"), length)) %>% 
  separate(age_at_med_start, 
           paste("age_at_med_start", 1:max(.$regimen_count), sep = ""), 
           sep = ";", remove = TRUE, extra = "warn", fill = "right") %>% 
  separate(medication, 
           paste("medication", 1:max(.$regimen_count), sep = ""), 
           sep = ";", remove = TRUE, extra = "warn", fill = "right") %>% 
  mutate(across(.cols = c(starts_with("age_at")), ~as.numeric(.))) %>% 
  mutate(drugs_ever = "Yes")

# Radiation
radiation <- Radiation %>% 
  mutate(across(.cols = c(age_at_radiation_start, age_at_radiation_stop), ~as.numeric(.))) %>% 
  mutate(had_radiation = case_when(
    !is.na(age_at_radiation_start) | 
      !is.na(age_at_radiation_stop)                            ~ "Yes",
    str_detect(rad_surgery_sequence,
               "before|after")                                 ~ "Yes",
    rad_surgery_sequence ==
      "No radiation therapy and/or surgical procedures"        ~ "No",
    str_detect(rad_reason_none_given, 
               "not administered")                             ~ "No",
    str_detect(rad_reason_none_given, 
               "not recommended")                              ~ "No"
  )) %>% 
  filter(had_radiation == "Yes" | is.na(had_radiation)) %>% 
  mutate(had_radiation = "Yes") %>% 
  # fill start date with stop date when start is not recorded
  mutate(age_at_radiation_start = coalesce(age_at_radiation_start, age_at_radiation_stop)) %>% 
  arrange(avatar_key, age_at_radiation_start, age_at_radiation_stop) %>% 
  distinct(avatar_key, age_at_radiation_start, age_at_radiation_stop, .keep_all = TRUE)

Radiation <- radiation %>% 
  group_by(avatar_key, age_at_radiation_start) %>% 
  summarise_at(vars(age_at_radiation_stop), paste, collapse = ", ") %>%
  group_by(avatar_key) %>% 
  summarise_at(vars(age_at_radiation_start, age_at_radiation_stop), paste, collapse = ";") %>%
  mutate(regimen_count = sapply(strsplit(age_at_radiation_start, ";"), length)) %>% 
  separate(age_at_radiation_start, 
           paste("age_at_radiation_start", 1:max(.$regimen_count), sep = ""), 
           sep = ";", remove = TRUE, extra = "warn", fill = "right") %>% 
  mutate(across(.cols = c(starts_with("age_at")), ~as.numeric(.))) %>% 
  mutate(radiation_ever = "Yes")

# SCT
sct <- SCT %>% 
  mutate(across(everything(), .fns = ~ na_if(., "Unknown/Not Applicable"))) %>% 
  filter(sct_ind == "Yes") %>% 
  # Fill age at transplant when not available
  mutate(age_at_transplant = coalesce(age_at_transplant, age_at_post_trans_status)) %>% 
  mutate(across(.cols = c(age_at_transplant), ~as.numeric(.))) %>% 
  arrange(avatar_key, age_at_transplant) %>% 
  distinct(avatar_key, age_at_transplant, .keep_all = TRUE)

SCT <- sct %>% 
  group_by(avatar_key) %>% 
  summarise_at(vars(age_at_transplant), paste, collapse = ";") %>%
  mutate(regimen_count = sapply(strsplit(age_at_transplant, ";"), length)) %>% 
  separate(age_at_transplant, 
           paste("age_at_transplant", 1:max(.$regimen_count), sep = ""), 
           sep = ";", remove = TRUE, extra = "warn", fill = "right") %>% 
  mutate(across(.cols = c(starts_with("age_at")), ~as.numeric(.))) %>% 
  mutate(sct_ever = "Yes")

# Outcome for progression
# Patient history for tobacco, alcohol ...

########################################################################################## III ### merge----
Global_data <- full_join(Germline, Vitals, by = "avatar_key") %>% 
  full_join(., Diagnosis_v4_7, by = "avatar_key") %>% 
  full_join(., Medication_v4_7, by = "avatar_key") %>% 
  full_join(., Radiation, by = "avatar_key") %>% 
  full_join(., SCT, by = "avatar_key")
write_rds(Global_data, "Global_data.rds")

germline_data <- Global_data %>% 
  filter(!is.na(Germline_SLID))
write_rds(germline_data, "germline_data.rds")


## END

