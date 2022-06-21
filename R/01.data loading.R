# import packages
library(tidyverse)
library(data.table)
library(lubridate)
library(gtsummary)

################################################################################################### I ### load data----

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                 "CH all tumor types", "raw data", "M2GEN")

path3 <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                 "CH all tumor types")

mrn1 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/MRN.csv"), na = c("PRBB-DO NOT USE")) %>% 
  filter(!is.na(MRN))
mrn2 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/MRN.csv"), na = c("PRBB-DO NOT USE")) %>% 
  filter(!is.na(MRN))
mrn <- bind_rows(mrn1, mrn2) %>% 
  distinct()
rm(mrn1, mrn2)
# myfiles = list.files(path = paste0(path,
#   "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v2 <- lapply(myfiles, read_csv)

# myfiles <- list.files(path = paste0(path,
#   "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v4 <- lapply(myfiles, read_csv)
# V2----
Demo_v2 <- read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/Demographics.csv"))
Vitals_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/DemographicsVitalStatusLastContact.csv"))
Medication_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/Medications.csv"),
           col_types = cols(AgeAtMedStart = col_character(),
                            AgeAtMedStartFlag = col_character(),
                            AgeAtMedStopFlag = col_character(),
                            AgeAtMedStop = col_character()))
Radiation_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/Radiation.csv"))
Metastasis_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/DiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Metastasis_v2_f <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/FollowUpDiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Staging_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/DiagnosisStaging.csv"), na = c("Not Available"),
           col_types = cols(AgeAtDiagnosisFlag = col_character(),
                            # AgeAtMedStartFlag = col_character(),
                            # AgeAtMedStopFlag = col_character(),
                            AgeAtPerformanceStatusAtDiagnosisFlag = col_character()))
Staging_v2_f <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/FollowUpDiagnosisStaging.csv"),
           col_types = cols(YearOfDiagnosis = col_double()))
Surgery_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/SurgeryBiopsy.csv"),
           col_types = cols(AgeAtSurgeryBiopsyFlag = col_character(),
                            AgeAtSurgeryBiopsy = col_character()))
TMarkers_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/TumorMarkers.csv"),
           col_types = cols(AgeAtTumorMarkerTestFlag = col_character()))
Sequencing_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/ClinicalSpecimen.csv")) %>% 
  select("AvatarKey", "ORIENSpecimenID", "WES", "AgeAtSpecimenCollection", "Tumor/Germline")

Indicators_v2 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/Indicators.csv")) %>% 
  select("AvatarKey", "COPDDiagnosisInd", "CVAStrokeDiagnosisInd", "InsulinDependentDiabetesMellitusDiagnosisInd", 
         "HypertensionDiagnosisInd", "MIHeartFailureDiagnosisInd", "VenousThrombosisDrugToxicityInd") %>% 
  distinct()
Follow_indicators_v2 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/FollowUpIndicators.csv")) %>% 
  select("AvatarKey", "COPDDiagnosisInd", "CVAStrokeDiagnosisInd", "InsulinDependentDiabetesMellitusDiagnosisInd", 
         "HypertensionDiagnosisInd", "MIHeartFailureDiagnosisInd", "VenousThrombosisDrugToxicityInd") %>% 
  distinct()
colnames(Follow_indicators_v2)[2:ncol(Follow_indicators_v2)] <- 
  paste("fw", colnames(Follow_indicators_v2)[2:ncol(Follow_indicators_v2)], sep = "_")

# V4----
Demo_v4 <- read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/PatientMaster.csv"))
Vitals_v4 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/VitalStatus.csv"))
Medication_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/Medications.csv"))
Radiation_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/Radiation.csv"))
Metastasis_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/MetastaticDisease.csv")) %>% 
  select("AvatarKey", "MetastaticDiseaseInd", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Staging_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/Diagnosis.csv"))
Surgery_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/SurgeryBiopsy.csv"))
TMarkers_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/TumorMarker.csv"))
Sequencing_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/ClinicalSpecimen.csv")) %>% 
  select("AvatarKey", "ORIENSpecimenID", "WES", "AgeAtSpecimenCollection", "Tumor/Germline")
SCT_v4 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/StemCellTransplant.csv"))

Indicators_v4 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/PatientHistory.csv")) %>% 
  select("AvatarKey", "SmokingStatus", "COPDDiagnosisInd", "CVAStrokeDiagnosisInd", "DVTDiagnosisInd",
         "HypercholesterolemiaDiagnosisInd", "HyperlipidemiaDiagnosisInd", 
         "InsulinDependentDiabetesMellitusDiagnosisInd", 
         "HypertensionDiagnosisInd", "HeartDiseaseDiagnosisInd", "MIHeartFailureDiagnosisInd",
         "PulmonaryEmbolismDiagnosisInd", "VenousThrombosisDrugToxicityInd")

# Demo with date
demo <- 
  readxl::read_xlsx(paste0(path, "/Yifen data/Demographics_Report.xlsx")) %>% 
  mutate(date_of_death = case_when(
    `Vital Status` == "Dead"          ~ `Vital Status Date`, 
    TRUE                              ~ NA_POSIXct_)) %>% 
  select(c("MRN", date_of_birth = `Date of Birth`, vital_status = `Vital Status`, "date_of_death",
           os_event_date = "Date of Last Contact or Death"))

# Samples
path1 <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                  "Jamie")
Clinical_linkage <- read.delim(paste0(path1, "/wes_somatic_mutations_metadata_v0.4.5.txt")) %>% 
  select(c("subject", SLID_germline, SLID_tumor, moffittSampleId, 
           moffittSampleId_tumor, moffittSampleId_germline,
           "ClinicalSpecimenLinkage_AgeAtSpecimenCollection",
           ClinicalSpecimenLinkage_HistologyBehavior, SpecimenDetail_DiseaseType)) %>% 
  arrange(subject, ClinicalSpecimenLinkage_AgeAtSpecimenCollection)
# Samples dates and Ids
sample_data <- 
  readxl::read_xlsx(paste0(path, "/Yifen data/Avatar SLIDs and 06Sdatesv2.xlsx"),
                    na = "NULL") %>% 
  select(-DateOfCollection,
         tumor_anatomic_site = "tumor anatomic site", germline_anatomic_site = "germline anatomic site")

# Jamila
# Cardiotoxic drugs and their manifestations
cardiotox <- 
  readxl::read_xlsx(paste0(path, "/Jamila Mammadova/data/Cardiotoxic drugs Jamila.xlsx"), na = "NA", n_max = 53)

cardiot_patients <- 
  readxl::read_xlsx(paste0(path, "/Jamila Mammadova/data/Cardio events RFs_Jamila_v2.xlsx"),
                    sheet = "Restructured_DCS", skip = 2, n_max = 150) %>% 
  select(-SN)

# write_rds(drugs_date, "drugs_date.rds")
drugs_date <- read_rds("drugs_date.rds")
drugs_date <- 
  readxl::read_xlsx(paste0(path, "/Jamila Mammadova/data/Data_For_Christelle.xlsx")) %>% 
  select(-SN) %>% 
  rename(D_TX_IN_jamila = D_TX_IN)
  
cardiot_patients <- cardiot_patients %>% full_join(., drugs_date, by = "MRN")







################################################################################################### I ### v4.7----
sample_data_v4_7 <- 
  readxl::read_xlsx(paste0(path3, "/raw data/Clinical and sample Data Nancy v4_7/v4.6and4.7/10R22000048_20220616_outfile.xlsx"),
                    sheet = "CDSC-AvatarMasterList_SDR-2 ",
                    na = "NULL") %>% 
  janitor::clean_names() %>% 
  filter(str_detect(tumor_germline_heme_project_id, "germline")) %>% 
  select(avatar_key = "orien_avatar_patient_id", "orien_specimen_id", "dna_sequencing_library_id",
         "mrn", "sample_family_id", "tumor_germline_heme_project_id",
         "specimen_site_of_collection", "date_of_specimen_collection")

cardiotox <- 
  readxl::read_xlsx(paste0(path, "/Jamila Mammadova/data/Cardiotoxic drugs Jamila.xlsx"), na = "NA", n_max = 53)

Diagnosis_v4_7 <- 
  readxl::read_xlsx(paste0(path3, "/raw data/Clinical and sample Data Nancy v4_7/v4.6and4.7/10R22000048_20220616_outfile.xlsx"),
                    sheet = "20220504_MCC_Diagnosis_V4 ") %>% 
  janitor::clean_names()
Diagnosis_v4_7 <- Diagnosis_v4_7 %>% 
  arrange(avatar_key, age_at_diagnosis) %>% 
  group_by(avatar_key) %>% 
  summarize_at(vars(age_at_diagnosis), str_c, collapse = "; ") %>% 
  ungroup() %>% 
  separate(age_at_diagnosis, into = paste("age_at_diagnosis_", 1:30, sep=""), sep = "; ", extra = "warn", 
                      fill = "right") %>% 
  purrr::keep(~!all(is.na(.))) %>% 
  mutate(across(c(starts_with("age_at_diagnosis_")), 
                ~ as.numeric(.)))

Medication_v4_7 <- 
  readxl::read_xlsx(paste0(path3, "/raw data/Clinical and sample Data Nancy v4_7/v4.6and4.7/10R22000048_20220616_outfile.xlsx"),
                    sheet = "20220504_MCC_Medications_V4") %>% 
  janitor::clean_names() %>% 
  select("avatar_key", "medication", "med_line_regimen", "age_at_med_start",
         "age_at_med_stop")

previous_list <- 
  read_csv(paste0(fs::path("","Volumes","Gillis_Research"), 
                           "/Jamila Mammadova/patients receiving cardiotoxic drugs with indicators.csv")) %>% 
  janitor::clean_names()
previous_list <- paste0(previous_list$avatar_key, collapse = "|")

previous_list1 <- 
  read_csv(paste0(fs::path("","Volumes","Gillis_Research"), 
                  "/Jamila Mammadova/List of new breast patients for Jamila 03012022.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(mrn = as.character(mrn))


# End Load