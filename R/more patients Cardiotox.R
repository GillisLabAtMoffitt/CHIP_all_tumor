# Find extra patients with WES from breast cancer sequential samples project

# import packages
library(tidyverse)

# I ### load data----

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "Breast CH")

breast_data <- 
  read_rds(paste0(path, "/processed data/blood_patients.rds"))

breast_samples <- 
  readxl::read_xlsx(
    paste0(path, "/output/Sample lists/TCSPL-ServiceRequestForm_MCC 21545 Gillis 02.24.22.xlsx"),
    sheet = "Sample List")

treatment <- 
  read_rds(paste0(path, "/processed data/treatment_long.rds"))


# I ### List---
breast_samples <- 
  breast_samples %>% 
  mutate(mrn = as.character(MRN))
treatment <- treatment %>% 
  filter(str_detect(mrn, paste0(breast_samples$mrn, collapse = "|"))) %>% 
  ungroup()


# List of drugs for Jamila
# write_csv(treatment %>% 
#             filter(treatment_type != "radioT") %>% 
#             select(treatment) %>% 
#             distinct(),
#           "List of drugs for Jamila.csv")


list_cardio_drugs <- paste("adriamycin; cyclophosphamide",
                       "pembrolizumab",
                       "adriamycin; cyclophosphamide; paclitaxel",
                       "pertuzumab; trastuzumab",
                       "5 fu; cyclophosphamide; epirubicin; docetaxel",
                       "trastuzumab",
                       "adriamycin; cyclophosphamide; docetaxel",
                       "adriamycin",
                       "carboplatin; docetaxel; traztuzumab",
                       "adriamycin; cyclophosphamide; vincristine",
                       "interferon; pertuzumab; trastuzumab", sep = "|")

new_cardio_patients <- treatment %>% 
  filter(str_detect(treatment, list_cardio_drugs))

new_cardio_patients <- breast_data %>% 
  filter(str_detect(mrn, paste0(new_cardio_patients$mrn, collapse = "|"))) %>% 
  select(mrn, deidentified_patient_id, date_of_birth, date_of_diagnosis1) %>% 
  distinct() %>% 
  left_join(., treatment %>% 
              select(mrn, treatment_start_date, treatment_end_date, treatment),
            by = "mrn")

write_csv(new_cardio_patients,
          "List of new patients for Jamila.csv")



