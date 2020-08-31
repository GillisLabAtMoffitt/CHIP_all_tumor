# import packages
library(tidyverse)
library(data.table)

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
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Medications.csv"),
           col_types = cols(AgeAtMedStart = col_character(),
                            AgeAtMedStop = col_character()))
Radiation_v2 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Radiation.csv"))
Metastasis_v2 <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/DiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Metastasis_v2_f <- 
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events/FollowUpDiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
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
  read_csv(paste0(path, "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events/MetastaticDisease.csv")) %>% 
  select("AvatarKey", "MetastaticDiseaseInd", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")

####################################################################################################### II ### Data cleaning----
# Demographics
Demographics <- bind_rows(Demo_v2, Demo_v4) %>%  # has no duplicate
  mutate(AgeAtFirstContact = case_when(
    AgeAtFirstContact == "Age 90 or Older" ~ 90,
    TRUE ~ as.numeric(AgeAtFirstContact)
  ))

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

# Medication----
# Binds V2_V4, keep distinct row, and cast
Medication_v2 <- Medication_v2 %>% 
  rename(MedLineRegimen = "TreatmentLineCodeKey") %>% 
  distinct(.)
Medication_v4 <- Medication_v4 %>% 
  distinct(.)
medication <- bind_rows(Medication_v2, Medication_v4, .id = "versionMed") %>% 
  distinct(.)
Medication <- dcast(
  setDT(medication),
  AvatarKey + AgeAtMedStart + CancerSiteForTreatment + CancerSiteForTreatmentCode +
    MedContinuing + AgeAtMedStop ~ rowid(AvatarKey),
  value.var = c("Medication")) %>% 
  unite(Medication, "1":ncol(.), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  arrange(AgeAtMedStart)
Medication <- dcast(setDT(Medication), AvatarKey ~ rowid(AvatarKey),
                     value.var = c("Medication", "AgeAtMedStart", "AgeAtMedStop", "CancerSiteForTreatment",
                                   "CancerSiteForTreatmentCode", "MedContinuing"), sep = "_regimen")
write_csv(Medication, paste0(path, "/output data/cleaned files/Medication.csv"))

# Radiation----
Radiation_v2 <- Radiation_v2 %>% 
  rename(RadPrimaryDiagnosisSiteCode = "RadSiteCode") %>% 
  rename(RadPrimaryDiagnosisSite = "RadSite") %>% 
  distinct(.)
Radiation_v4 <- Radiation_v4 %>% 
  distinct(.)
radiation <- bind_rows(Radiation_v2, Radiation_v4, .id = "versionRad") %>% 
  distinct(.)
Radiation <- dcast(setDT(radiation), AvatarKey+AgeAtRadiationStart+AgeAtRadiationStop+
                     RadSurgerySequence+RadTreatmentLine ~ rowid(AvatarKey),
                    value.var = c("AgeAtRadiationStopFlag", "RadModality", "RadDose", "RadSite",
                                  "RadPrimaryDiagnosisSiteCode", "RadPrimaryDiagnosisSite",
                                  "RadFractions"), sep = "_sequence")

Radiation <- dcast(setDT(Radiation), AvatarKey ~ rowid(AvatarKey),
                   value.var = c("AgeAtRadiationStart", "AgeAtRadiationStop", "RadSurgerySequence", 
                                 "RadTreatmentLine",
                                 "AgeAtRadiationStopFlag_sequence1", "AgeAtRadiationStopFlag_sequence2",
                                 "AgeAtRadiationStopFlag_sequence3", "AgeAtRadiationStopFlag_sequence4",
                                 "RadModality_sequence1", "RadModality_sequence2",
                                 "RadModality_sequence3", "RadModality_sequence4",
                                 "RadDose_sequence1", "RadDose_sequence2", "RadDose_sequence3", "RadDose_sequence4",
                                 "RadSite_sequence1", "RadSite_sequence2", "RadSite_sequence3", "RadSite_sequence4",
                                 "RadPrimaryDiagnosisSiteCode_sequence1", "RadPrimaryDiagnosisSiteCode_sequence2",
                                 "RadPrimaryDiagnosisSiteCode_sequence3", "RadPrimaryDiagnosisSiteCode_sequence4",
                                 "RadPrimaryDiagnosisSite_sequence1", "RadPrimaryDiagnosisSite_sequence2",
                                 "RadPrimaryDiagnosisSite_sequence3", "RadPrimaryDiagnosisSite_sequence4",
                                 "RadFractions_sequence1", "RadFractions_sequence2",
                                 "RadFractions_sequence3", "RadFractions_sequence4"), sep = "_regimen")
write_csv(Radiation, paste0(path, "/output data/cleaned files/Radiation.csv"))

# Metastasis----
# remove the ID with follow up from the base file
# So take Yes, No, Unknown independently 
# remove the ID of Yes from No then Yes+No from Unknown
Metastasis <- bind_rows(Metastasis_v2, Metastasis_v2_f, Metastasis_v4, .id = "versionMets")
metastasis_Yes <- Metastasis %>% 
  filter(str_detect(MetastaticDiseaseInd, "Yes"))
uid <- paste(unique(metastasis_Yes$AvatarKey), collapse = "|")
Metastasis_No <- Metastasis %>% 
  filter(str_detect(MetastaticDiseaseInd, "No"))
Metastasis_No <- Metastasis_No[(!grepl(uid, Metastasis_No$AvatarKey)),]
metastasis <- bind_rows(metastasis_Yes, Metastasis_No)
uid <- paste(unique(metastasis$AvatarKey), collapse = "|")
Metastasis_Unknown <- Metastasis %>% 
  filter(str_detect(MetastaticDiseaseInd, "Unknown"))
Metastasis_Unknown <- Metastasis_Unknown[(!grepl(uid, Metastasis_Unknown$AvatarKey)),]
metastasis <- bind_rows(metastasis, Metastasis_Unknown) %>% 
  distinct(.)
Metastasis <- dcast(setDT(metastasis), AvatarKey ~ rowid(AvatarKey),
                   value.var = c("MetastaticDiseaseInd", 
                                 "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")) 
write_csv(Metastasis, paste0(path, "/output data/cleaned files/Metastasis.csv"))

# Cleaning
rm(Medication_v2, Medication_v4, Radiation_v2, Radiation_v4, Metastasis_No,
   Metastasis_Unknown, Metastasis_v2, Metastasis_v2_f, Metastasis_v4, metastasis_Yes)

# Staging----

















# End