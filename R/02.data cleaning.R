################################################################################################### I ### With updated data----
# Demographics----
demo <- 
  full_join(mrn, demo, by = "MRN")
Demo_v2 <- Demo_v2 %>% 
  drop_na(all_of(c("RecordKey" , "row_id")))
Demo_v4 <- Demo_v4 %>% 
  drop_na(all_of(c("AgeAtClinicalRecordCreation", "row_id")))

Demographics <- Demo_v4 %>% 
  # V2 and V4 have no duplicate and 
  # no patients in common when NA removed
  bind_rows(.,  Demo_v2 %>% 
              rename(AgeAtClinicalRecordCreation = AgeAtFirstContact,
                     YearOfClinicalRecordCreation = AgeAtFirstContactFlag)
            ) %>% 
  # Add dates for back calculation
  # 7 patients are absent from Yifen's data vs Garrick's
  left_join(demo, ., by = c("mrn" = "MRN"))


# WES----
# v4.7
Germline_v4.7 <- sample_data_v4_7_dates %>% 
  filter(str_detect(disease_type_conformed, "germline") &
           specimen_site_of_collection == "Blood") %>%
  mutate(germline_collection_date = date_of_specimen_collection) %>% 
  select(avatar_key = "orien_avatar_patient_id", mrn, 
         germline_orien_id = orien_specimen_id, 
         SLID_germline = dna_sequencing_library_id,
         germline_sample_family_id = sample_family_id,
         germline_site_of_collection = specimen_site_of_collection, 
         germline_collection_date,
         date_of_specimen_collection,
         dob)

Tumor_v4.7 <- sample_data_v4_7_dates %>% 
  filter(!str_detect(disease_type_conformed, "germline")) %>%
  mutate(tumor_collection_date = date_of_specimen_collection) %>% 
  select(avatar_key = "orien_avatar_patient_id", mrn, 
         tumor_orien_id = orien_specimen_id, 
         dna_sequencing_library_id, rna_sequencing_library_id,
         tumor_sample_family_id = sample_family_id,
         tumor_site_of_collection = specimen_site_of_collection, 
         tumor_collection_date,
         date_of_specimen_collection,
         disease_type_conformed, histology, release,
         dob, date_of_diagnosis, age_at_diagnosis)

WES_v4.7 <- full_join(Germline_v4.7, Tumor_v4.7, 
                      by = c("avatar_key", "mrn", "dob")) %>% 
  mutate

path3 <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                  "CH all tumor types")
write_csv(WES_v4.7, paste0(path3, "/processed WES ids list/matched germline tumor sample ids all tumor type v04.7.csv"))

################################################################################################### II ### With older data----

# Cardiotoxicities
cardiotox <- cardiotox %>% 
  `colnames<-`(str_replace_all(colnames(.), " ", "_")) %>% 
  fill(Chemoterapeutical_drug, Incidence_rate, Cardiovascular_manifestations, .direction = "down") %>% 
  mutate(Drug_names = tolower(Drug_names),
         Drug_names = str_replace_all(Drug_names, ", and | and |, ", "|"), 
         
         Drug_names = str_remove(Drug_names, "5-|.*: "), 
         Drug_names = gsub("\\(.*?)", "", Drug_names),
         Drug_names = str_replace_all(Drug_names, "interferon-alfa", "interferon alfa")) 
# mutate(across(everything(), ~na_if(., "NA")))

# Demographics----
demo <- 
  full_join(mrn, demo, by = "MRN")
Demographics <- 
  bind_rows(Demo_v2, Demo_v4) %>%  # has no duplicate
  left_join(demo, ., by = "AvatarKey") %>% 
  # mutate(AgeAtFirstContact = case_when(
  #   AgeAtFirstContact == "Age 90 or Older" ~ 90,
  #   TRUE ~ as.numeric(AgeAtFirstContact)
  # )) %>% 
  mutate(Race = case_when(
    str_detect(Race, "American")                              ~ "American Indian",
    str_detect(Race, "Asian|Chinese|Filipino|Pakistani|Polynesian|Vietnamese")
    ~ "Asian",
    str_detect(Race, "Unknown")                               ~ "Unknown",
    TRUE                                                      ~ Race
  )) %>% 
  mutate(Race = factor(Race, 
                       levels = c("White", "Black", "Other", "Asian", "Unknown", "American Indian"))) %>% 
  mutate(Ethnicity = case_when(
    Ethnicity %in% c("Non-Spanish; Non-Hispanic", "Spanish Surname Only")            ~ "Non-Spanish",
    str_detect(Ethnicity, "Mexican|Cuban|Domincian|Spanish|Puerto|American")         ~ "Spanish",
    str_detect(Ethnicity, "Unknown")                                                 ~ "Unknown",
    TRUE                                                                             ~ Ethnicity
  )) %>% 
  mutate(Ethnicity = factor(Ethnicity, levels = c("Non-Spanish", "Spanish", "Unknown")))

rm(demo, Demo_v2, Demo_v4, mrn)  
# WES
Clinical_linkage <- full_join(sample_data, Clinical_linkage,
                              by = c("subject", "SLID_germline", "SLID_tumor",
                                     "moffittSampleId", "moffittSampleId_tumor", 
                                     "moffittSampleId_germline")) %>% 
  filter(germline_anatomic_site == "Blood") %>% 
  left_join(., Demographics %>% select(AvatarKey, date_of_birth),
            by = c("subject" = "AvatarKey")) %>% 
  # Calculate date of tumor collection when absent
  mutate(ClinicalSpecimenLinkage_AgeAtSpecimenCollection = as.numeric(ClinicalSpecimenLinkage_AgeAtSpecimenCollection)) %>% 
  mutate(days_calc_365 = ClinicalSpecimenLinkage_AgeAtSpecimenCollection * 365) %>% 
  mutate(tunor_collection_dt = date_of_birth + days_calc_365) %>% 
  mutate(tunor_collection_dt = coalesce(DateOfCollection_tumor, as.Date(tunor_collection_dt))) %>% 
  mutate(interval_germ_tumor = abs(interval(start= DateOfCollection_germline, end= tunor_collection_dt) /
                                     duration(n=1, unit="days"))) %>% 
  arrange(subject, interval_germ_tumor) %>% 
  group_by(subject) %>% 
  # distinct(subject, moffittSampleId_germline, .keep_all = TRUE)
  mutate(n = row_number(subject)) %>% 
  ungroup() %>% 
  mutate(is_tumor_closest_to_germline = case_when(
    n == 1       ~ "Yes",
    TRUE         ~ "No"
  )) %>% 
  filter(is_tumor_closest_to_germline == "Yes") %>% 
  select(-c(n, is_tumor_closest_to_germline, days_calc_365, date_of_birth))

# List samples for CHIP analysis closest tumor to germline for each patient
write_csv(Clinical_linkage %>% right_join(mrn, ., by = c("AvatarKey" = "subject")) %>% 
            select(MRN, AvatarKey, 
                   tumor_anatomic_site,
                   ClinicalSpecimenLinkage_HistologyBehavior, SpecimenDetail_DiseaseType,
                   "SLID_germline", "moffittSampleId_germline", 
                   DateOfCollection_germline, 
                   "moffittSampleId", 
                   "SLID_tumor", "moffittSampleId_tumor", 
                   tunor_collection_dt,
                   ClinicalSpecimenLinkage_AgeAtSpecimenCollection
            ),
          paste0(path, "/output data/cleaned files/Clinical_linkage with dates.csv"))

write_csv(cardiot_patients %>% left_join(., mrn, by = "MRN") %>% 
            select(MRN, AvatarKey, "D_TX_IN_jamila", "D_CTOX_IN") %>% 
            left_join(., Clinical_linkage, by = c("AvatarKey" = "subject")) %>% 
            select(MRN, AvatarKey, 
                   tumor_anatomic_site,
                   ClinicalSpecimenLinkage_HistologyBehavior, SpecimenDetail_DiseaseType,
                   "SLID_germline", "moffittSampleId_germline", 
                   DateOfCollection_germline, 
                   "moffittSampleId", 
                   "SLID_tumor", "moffittSampleId_tumor", 
                   tunor_collection_dt,
                   ClinicalSpecimenLinkage_AgeAtSpecimenCollection,
                   "D_TX_IN_jamila", "D_CTOX_IN"),
          paste0(path, "/output data/cleaned files/Sample data tumor closest to germline Jamila patients.csv"))









# Vitals----
Vitals_v2$AvatarKey[which(duplicated(Vitals_v2$AvatarKey))]

## PROBLEM : Found patient with 2 age at date, ex: A007107 ##
# Vitals_v2_test <- Vitals_v2 %>%
#   group_by(AvatarKey) %>%
#   mutate(death_max = max(AgeAtDeath)) %>% 
#   select(AvatarKey, AgeAtDeath, AgeAtDeath_av, AgeAtDeath_bids, AgeAtDeathFlag, death_max, everything())
# a <- Vitals_v2_test$AvatarKey[which(Vitals_v2_test$AgeAtDeath != Vitals_v2_test$death_max)]


Vitals_v2 <- Vitals_v2 %>% 
  arrange(desc(VitalStatus) , AvatarKey, desc(EstAgeAtLastContact)) %>% 
  # arrange(desc(VitalStatusConfirmed) , AgeAtDeath, EstAgeAtLastContact) %>% # Will keep the earliest date A016365
  distinct(AvatarKey, .keep_all = TRUE) %>% 
  rename(AgeAtLastContact = "EstAgeAtLastContact")

Vitals_v4$AvatarKey[which(duplicated(Vitals_v4$AvatarKey))]
b <- paste0(Vitals_v4$AvatarKey[which(duplicated(Vitals_v4$AvatarKey))], collapse = "|")
duplicate <- Vitals_v4[grepl(b, Vitals_v4$AvatarKey),]
Vitals_v4 <- Vitals_v4 %>%
  arrange(desc(VitalStatus) , AvatarKey, desc(AgeAtLastContact)) %>% 
  distinct(AvatarKey, .keep_all = TRUE)
# Same problem
# Vitals_v4_test <- Vitals_v4 %>%
#   group_by(AvatarKey) %>% 
#   mutate(death_max = max(AgeAtDeath))
# a <- Vitals_v4_test$AvatarKey[which(Vitals_v4_test$AgeAtDeath != Vitals_v4_test$death_max)]


# Remove id from v4 in v2 -> Just to make sure
uid <- paste(unique(Vitals_v4$AvatarKey), collapse = "|")
Vitals_v2 <- Vitals_v2[(!grepl(uid, Vitals_v2$AvatarKey)),]
# Bind
Vitals <- bind_rows(Vitals_v2, Vitals_v4, .id = "version_vital") %>% 
  select(c("version_vital", "AvatarKey", "VitalStatus", "AgeAtDeath", "YearOfDeath", "CauseOfDeath", 
           "VitalStatus_av", "AgeAtDeath_av", "AgeAtDeath_bids", 
           "VitalStatusConfirmed", "VitalStatusSource", "AgeAtLastContact"))

# Medication----
# Binds V2_V4, keep distinct row, and cast
Medication_v2 <- Medication_v2 %>% 
  rename(MedLineRegimen = "TreatmentLineCodeKey", MedPrimaryDiagnosisSiteCode = "CancerSiteForTreatmentCode", 
         MedPrimaryDiagnosisSite = "CancerSiteForTreatment") %>% 
  select(-c("RecordKey", "AgeAtMedStartFlag", "YearOfMedStart", "AgeAtMedStopFlag", "row_id"))
Medication_v4 <- Medication_v4 %>%
  select(-c("MedicationInd", "MedReasonNoneGiven", "AgeAtMedStartFlag", "YearOfMedStart", "YearOfMedStart", 
            "AgeAtMedStopFlag", "YearOfMedStop", "SystemicSurgerySequence", "row_id"))

medication <- bind_rows(Medication_v2, Medication_v4, .id = "version") %>% 
  distinct() %>% 
  mutate(Medication = tolower(Medication), MedPrimaryDiagnosisSite = tolower(MedPrimaryDiagnosisSite))

# dcast per regimen first then per patient
Medication1 <- dcast(setDT(medication),
                     AvatarKey + MedLineRegimen + AgeAtMedStart + MedPrimaryDiagnosisSite + MedPrimaryDiagnosisSiteCode +
                       MedContinuing + AgeAtMedStop ~ rowid(AvatarKey), 
                     value.var = c("Medication")) %>% 
  unite(Medication, "1":ncol(.), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  arrange(AgeAtMedStart)
Medication <- dcast(setDT(Medication1), AvatarKey ~ rowid(AvatarKey),
                    value.var = c("Medication", "AgeAtMedStart", "AgeAtMedStop", "MedPrimaryDiagnosisSite",
                                  "MedPrimaryDiagnosisSiteCode", "MedContinuing"), sep = "_regimen")
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
# Some patients show "Yes", "No" AND "Unknown" so may be because no date of follow up
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
  mutate(AgeAtPerformStatusMostRecent = case_when(# Will check for more cleaning---- Pb with class(YearOfDiagnosis)
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
staging <- bind_rows(Staging_v2 %>% select(AvatarKey, AgeAtDiagnosis), Staging_v4 %>% select(AvatarKey, AgeAtDiagnosis)) %>% 
  distinct() %>% 
  filter(!is.na(AgeAtDiagnosis)) %>% 
  arrange(AvatarKey)



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
  full_join(., TMarkers, by= "AvatarKey") %>% 
  left_join(., Clinical_linkage %>% select(c("subject", "ClinicalSpecimenLinkage_DiseaseType")), by= c("AvatarKey" = "subject"))

cardiot_patients <- left_join(mrn, Clinical_linkage, by = c("AvatarKey" = "subject")) %>% 
  right_join(., cardiot_patients, by = "MRN")



# End Cleaning
