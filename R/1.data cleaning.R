# import packages
library(tidyverse)
library(data.table)

################################################################################################### I ### load data----

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                 "M2GEN")

# myfiles = list.files(path = paste0(path,
#   "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v2 <- lapply(myfiles, read_csv)

# myfiles <- list.files(path = paste0(path,
#   "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v4 <- lapply(myfiles, read_csv)
# V2----
Demo_v2 <- read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Demographics.csv"))
Vitals_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/DemographicsVitalStatusLastContact.csv"))
Medication_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Medications.csv"),
           col_types = cols(AgeAtMedStart = col_character(),
                            AgeAtMedStop = col_character()))
Radiation_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/Radiation.csv"))
Metastasis_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/DiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Metastasis_v2_f <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/FollowUpDiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Staging_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/DiagnosisStaging.csv"))
Staging_v2_f <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/FollowUpDiagnosisStaging.csv"))
Surgery_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/SurgeryBiopsy.csv"),
           col_types = cols(AgeAtSurgeryBiopsyFlag = col_character()))
TMarkers_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/TumorMarkers.csv"),
           col_types = cols(AgeAtTumorMarkerTestFlag = col_character()))
Sequencing_v2 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/ClinicalSpecimen.csv")) %>% 
  select("AvatarKey", "ORIENSpecimenID", "WES", "AgeAtSpecimenCollection", "Tumor/Germline")

# V4----
Demo_v4 <- read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/PatientMaster.csv"))
Vitals_v4 <-
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/VitalStatus.csv"))
Medication_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/Medications.csv"))
Radiation_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/Radiation.csv"))
Metastasis_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/MetastaticDisease.csv")) %>% 
  select("AvatarKey", "MetastaticDiseaseInd", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Staging_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/Diagnosis.csv"))
Surgery_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/SurgeryBiopsy.csv"))
TMarkers_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/TumorMarker.csv"))
Sequencing_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/ClinicalSpecimen.csv")) %>% 
  select("AvatarKey", "ORIENSpecimenID", "WES", "AgeAtSpecimenCollection", "Tumor/Germline")
SCT_v4 <- 
  read_csv(paste0(path, "/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/StemCellTransplant.csv"))


################################################################################################### II ### Data cleaning----
# Demographics----
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
  rename(MedLineRegimen = "TreatmentLineCodeKey")

medication <- bind_rows(Medication_v2, Medication_v4) %>% 
  select(-c(RecordKey, row_id)) %>% 
  arrange(ChangeOfTreatment) %>% 
  distinct(AvatarKey, Medication, AgeAtMedStart, MedLineRegimen, MedContinuing, AgeAtMedStop, ChangeOfTreatment,
           .keep_all = TRUE)
medication$AvatarKey[which(duplicated(medication[c("AvatarKey", "Medication", "AgeAtMedStart", 
                                                   "MedLineRegimen", "MedContinuing", "AgeAtMedStop")]))]
# Have few duplicate but will be remove in the next step
Medication <- dcast(setDT(medication),
  AvatarKey + AgeAtMedStart + CancerSiteForTreatment + CancerSiteForTreatmentCode +
    MedContinuing + AgeAtMedStop ~ rowid(AvatarKey), 
  value.var = c("Medication")) %>% 
  unite(Medication, "1":ncol(.), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  arrange(AgeAtMedStart)
Medication <- dcast(setDT(Medication), AvatarKey ~ rowid(AvatarKey),
                     value.var = c("Medication", "AgeAtMedStart", "AgeAtMedStop", "CancerSiteForTreatment",
                                   "CancerSiteForTreatmentCode", "MedContinuing"), sep = "_regimen")
# write_csv(Medication, paste0(path, "/output data/cleaned files/Medication.csv"))

# Radiation----
Radiation_v2 <- Radiation_v2 %>% 
  rename(RadPrimaryDiagnosisSiteCode = "RadSiteCode") %>% 
  rename(RadPrimaryDiagnosisSite = "RadSite")
radiation <- bind_rows(Radiation_v2, Radiation_v4, .id = "versionRad") %>% 
  distinct(AvatarKey, AgeAtRadiationStart, RadPrimaryDiagnosisSiteCode, RadPrimaryDiagnosisSite, 
           RadTreatmentLine, RadDose, RadSite, .keep_all = TRUE)

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
# write_csv(Radiation, paste0(path, "/output data/cleaned files/Radiation.csv"))

# Metastasis----
# remove the ID with follow up from the base file
# So take Yes, No, Unknown independently 
# remove the ID of Yes from No then Yes+No from Unknown
Metastasis <- bind_rows(Metastasis_v2, Metastasis_v2_f, Metastasis_v4, .id = "versionMets") # Will check for more cleaning----
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
# write_csv(Metastasis, paste0(path, "/output data/cleaned files/Metastasis.csv"))

# Staging----
Staging_v2 <- Staging_v2 %>% 
  mutate(AgeAtPerformStatusMostRecent = case_when(# Will check for more cleaning----
    AgeAtPerformStatusMostRecent == "Age 90 or Older" ~ 90,
    TRUE ~ as.numeric(AgeAtPerformStatusMostRecent)
    )) %>% 
  mutate(AgeAtDiseaseStatus = case_when(
    AgeAtDiseaseStatus == "Age 90 or Older" ~ 90,
    TRUE ~ as.numeric(AgeAtDiseaseStatus)
  )) %>% 
  mutate(staging_done = case_when(
    !is.na(TNMEditionNumber) &
      !is.na(OtherStagingSystem) ~ "done"
  ))

staging <- bind_rows(Staging_v2, Staging_v2_f, Staging_v4, .id = "version") %>%  # Need more cleaning for when followup
  select(-c(RecordKey, row_id)) %>% 
  arrange(AgeAtDiagnosis)

staging$AvatarKey[which(duplicated(staging$AvatarKey))]

Staging <- dcast(setDT(staging), AvatarKey ~ rowid(AvatarKey),
                    value.var = c("PrimaryDiagnosisSiteCode", "PrimaryDiagnosisSite", "AgeAtDiagnosis",
                                  "DiagnosedLastSixMonths", "Histology", "HistologyCode", "ClinTStage", 
                                  "ClinNStage", "ClinMStage", "TNMEditionNumber", "OtherStagingSystem", 
                                  "OtherStagingGradeValue","staging_done")) 

# write_csv(Staging, paste0(path, "/output data/cleaned files/Staging.csv"))

# Surgery----
Surgery_v2 <- Surgery_v2 %>% 
  rename(SiteTherapeutic = "SurgeryBiopsyType") %>% 
  mutate(AgeAtSurgeryBiopsy = case_when(
    AgeAtSurgeryBiopsy == "Age 90 or Older" ~ 90,
    TRUE ~ AgeAtSurgeryBiopsy
  ))
Surgery_v4 <- Surgery_v4 %>% 
  mutate(AgeAtSurgeryBiopsy = case_when(
    AgeAtSurgeryBiopsy == "Age 90 or Older" ~ 90,
    TRUE ~ as.numeric(AgeAtSurgeryBiopsy)
  ))
surgery <- bind_rows(Surgery_v2, Surgery_v4, .id = "version") %>% 
  filter(SiteTherapeutic %in% c("Therapeutic", "Yes")) %>%
  select(-c(RecordKey, row_id)) %>% 
  distinct(.)

Surgery <- dcast(setDT(surgery), AvatarKey+AgeAtSurgeryBiopsy ~ rowid(AvatarKey),
                 value.var = c("SurgeryBiopsyLocation", "SurgeryBiopsyLocationCode",
                               "PrimaryDiagnosisSiteCode", "PrimaryDiagnosisSite", 
                               "MethodSurgicalResection"), sep = "_site") %>% 
  unite(SurgeryBiopsyLocation, starts_with("SurgeryBiopsyLocation_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite(SurgeryBiopsyLocationCode, starts_with("SurgeryBiopsyLocationCode_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite(PrimaryDiagnosisSiteCode, starts_with("PrimaryDiagnosisSiteCode_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite(PrimaryDiagnosisSite, starts_with("PrimaryDiagnosisSite_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite(For_Resection, starts_with("MethodSurgicalResection_"), sep = "; ", na.rm = TRUE, remove = TRUE)
Surgery <- dcast(setDT(surgery), AvatarKey ~ rowid(AvatarKey),
                 value.var = c("AgeAtSurgeryBiopsy", "SurgeryBiopsyLocation", "SurgeryBiopsyLocationCode", 
                               "PrimaryDiagnosisSiteCode", "PrimaryDiagnosisSite",
                               "MethodSurgicalResection"), sep = "_regimen") 
# write_csv(Surgery, paste0(path, "/output data/cleaned files/Surgery.csv"))

# Tumor markers----
TMarkers_v2 <- TMarkers_v2 %>% # Need more cleaning for when followup
  mutate(TMarkerLowRange = case_when(
    TMarkerLowRange == "0.0 TO 35.0" ~ 35,
    TRUE ~ as.numeric(TMarkerLowRange)
  )) %>% 
  mutate(TMarkerHighRange = case_when(
    TMarkerHighRange == "35.1 AND UP" ~ 35.1,
    TRUE ~ as.numeric(TMarkerHighRange)
  ))
tmarkers <- bind_rows(TMarkers_v2, TMarkers_v4, .id = "version")
TMarkers <- dcast(setDT(tmarkers), AvatarKey ~ rowid(AvatarKey),
                 value.var = c("TMarkerTest", "AgeAtTumorMarkerTest", "AgeAtTumorMarkerTestFlag",
                               "TMarkerResult", "TMarkerResultValue", "TMarkerValueUOM", "TMarkerRangeIndicator",
                               "TMarkerLowRange", "TMarkerHighRange", "TMarkerPercentStain", 
                               "TMarkerPercentStainResultValue", "TumorMarkerInd", "TumorMarkerKey")) 
# write_csv(TMarkers, paste0(path, "/output data/cleaned files/Tumor markers.csv"))

# Sequencing----
Sequencing_v2 <- Sequencing_v2 %>% filter(`Tumor/Germline`== "Germline")
Sequencing_v4 <- Sequencing_v4 %>% filter(`Tumor/Germline`== "Germline")

Sequencing <- bind_rows(Sequencing_v2, Sequencing_v4) %>% 
  unique()
Sequencing[duplicated(Sequencing$AvatarKey),] %>% arrange(AvatarKey)

# SCT----
SCT_v4 <- SCT_v4 %>% # Need more cleaning for when followup
  select(-row_id) %>% 
  filter(SCTInd == "Yes") %>% 
  distinct(AvatarKey, AgeAtTransplant, .keep_all = TRUE) %>% 
  arrange(AgeAtTransplant)
SCT <- dcast(setDT(SCT_v4), AvatarKey ~ rowid(AvatarKey),
             value.var = c("AgeAtTransplant", "AgeAtTransplantFlag", "SCTPrimaryDiagnosisSiteCode",
                           "SCTPrimaryDiagnosisSite", "TransplantType", "TransplantCellSource",
                           "AgeAtPostTransStatus", "MRDTestInd", "AgeAtMRDTest")) 




# Cleaning
rm(Demo_v2, Demo_v4, Vitals_v2, Vitals_v2a, Vitals_v4,
   Medication_v2, Medication_v4, Radiation_v2, Radiation_v4, Metastasis_No,
   Metastasis_Unknown, Metastasis_v2, Metastasis_v2_f, Metastasis_v4, metastasis_Yes,
   Staging_v2, Staging_v2_f, Staging_v4, Surgery_v2, Surgery_v4, TMarkers_v2, TMarkers_v4, 
   Sequencing_v2, Sequencing_v4, SCT_v4, uid)
################################################################################################### III ### Merging----

Global_data <- full_join(Sequencing, Demographics, by= "AvatarKey") %>% # Vitals
  full_join(., Staging, by= "AvatarKey") %>% 
  full_join(., Medication, by= "AvatarKey") %>% 
  full_join(., SCT, by= "AvatarKey") %>% 
  full_join(., Radiation, by= "AvatarKey") %>% 
  full_join(., Surgery, by= "AvatarKey") %>% 
  full_join(., Metastasis, by= "AvatarKey") %>% 
  full_join(., TMarkers, by= "AvatarKey")













# End