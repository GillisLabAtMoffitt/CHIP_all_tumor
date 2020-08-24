# import packages
library(tidyverse)

####################################################################################################### I ### load data----

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                 "M2GEN")

# myfiles = list.files(path = paste0(path,
#   "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v2 <- lapply(myfiles, read_csv)

# myfiles <- list.files(path = paste0(path,
#   "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v4 <- lapply(myfiles, read_csv)
# V2
Demo_v2 <- read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Demographics.csv"))
Vitals_v2 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/DemographicsVitalStatusLastContact.csv"))
Medication_v2 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Medications.csv"))
Radiation_v2 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Radiation.csv"))
Metastasis_v2 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/DiagnosisMetastaticDisease.csv"))
Metastasis_v2_f <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/FollowUpDiagnosisMetastaticDisease.csv"))
Staging_v2 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/DiagnosisStaging.csv"))
Staging_v2_f <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/FollowUpDiagnosisStaging.csv"))

# V4
Demo_v4 <- read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events/PatientMaster.csv"))
Vitals_v4 <-
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events/VitalStatus.csv"))
Medication_v4 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events/Medications.csv"))
Radiation_v4 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events/Radiation.csv"))
Metastasis_v4 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events/MetastaticDisease.csv"))

####################################################################################################### II ### Data cleaning----
# Demographics
Demographics <- bind_rows(Demo_v2, Demo_v4) # has no duplicate

# Vitals
Vitals_v2$AvatarKey[which(duplicated(Vitals_v2$AvatarKey))]
Vitals_v2a <- Vitals_v2 %>% 
  arrange(desc(VitalStatusConfirmed) , AgeAtDeath, EstAgeAtLastContact) %>% # Will keep the earliest date A016365
  distinct(AvatarKey, .keep_all = TRUE)

Vitals_v4$AvatarKey[which(duplicated(Vitals_v4$AvatarKey))]
# Same problem

# Do we have same id in v2 and v4 -> No, so good
uid <- paste(unique(Vitals_v2$AvatarKey), collapse = "|")
Vitals_v4 <- Vitals_v4[(!grepl(uid, Vitals_v4$AvatarKey)),]

# For medication
# 1. Binds V2_V4, keep distinct row
Medication_v2 <- Medication_v2 %>% 
  rename(MedLineRegimen = "TreatmentLineCodeKey")
Medication_v4 <- Medication_v4 %>% 
  mutate_at(vars(starts_with("Age")), ~ as.double(.))
Medication <- bind_rows(Medication_v2, Medication_v4, .id = "versionMed")

colnames(Medication_v2)




