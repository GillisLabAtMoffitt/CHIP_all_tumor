####################################################################################################### I ### Clinical mining----


# Will need to mofy dataframe name later when everything is merge

# Age per Gender
Demographics %>% 
  ggplot(aes(x= Sex, y=AgeAtFirstContact), fill= Sex)+
  geom_boxplot(color= c("purple3", "royalblue2")) +
  labs(x="Gender", y="Age at First Contact", title="Age repartition per gender")

# Age per Race
Demographics %>% 
  ggplot(aes(x= Race, y=AgeAtFirstContact), fill= Race)+
  geom_boxplot() +
  coord_flip() +
  labs(x="Gender", y="Age at First Contact", title="Age repartition per gender")

# Prep _______TO PLACE BEFORE
Global_data <- Global_data %>% 
  mutate(AgeAtDiagnosis_1 = as.numeric(AgeAtDiagnosis_1)) %>% 
  # mutate(Race = case_when(
  #   Race %in% c("Asian Indian", "Asian Indian or Pakistani, NOS", 
  #               "Asian Indian Or Pakistani, Nos (Code 09 Prior To Version 12)",
  #               "Chinese", "Filipino",
  #               "Other Asian, including Asian, NOS and Oriental, NOS",
  #               "Pakistani", "Polynesian, NOS", "Vietnamese")                      ~ "Asian",
  #   Race %in% c("American Indian, Aleutian, or Eskimo (includes all indigenous populations of the Western hemisphere)",
  #               "American Indian, Aleutian, Or Eskimo (Includes All Indigenous Populations Of The Western Hemisphere)")
  #                                                                                  ~ "American Indian",
  #   Race %in% c("Unknown", "Unknown/Not Reported")                                 ~ "Unknown",
  #   TRUE ~ Race
  # )) %>% 
  # mutate(Race = factor(Race, levels = c("White", "Black", "Other", "Asian", "Unknown", "American Indian"))) %>% 
  # mutate(Ethnicity = case_when(
  #   Ethnicity %in% c("Cuban", "Domincian Republic", "Mexican (includes Chicano)", "Mexican (Includes Chicano)",
  #                   "Other specified Spanish/Hispanic origin (includes European; excludes Dominican Republic)",
  #                   "Other Specified Spanish/Hispanic Origin (Includes European; Excludes Dominican Republic)",
  #                   "Puerto Rican", "South or Central American (except Brazil)", "South Or Central American (Except Brazil)"                    )                                                               ~ "Spanish",
  #   Ethnicity %in% c("Non-Spanish; Non-Hispanic", "Spanish Surname Only",
  #                   "Spanish, Nos; Hispanic, Nos; Latino, Nos", 
  #                   "Spanish, NOS; Hispanic, NOS; Latino, NOS")                     ~ "Non-Spanish",
  #   Ethnicity %in% c("Unknown", "Unknown/Not Reported")                             ~ "Unknown",
  #   TRUE                                                                            ~ Ethnicity
  #   )) %>% 
  # mutate(Ethnicity = factor(Ethnicity, levels = c("Non-Spanish", "Spanish", "Unknown")))


sum(is.na(Global_data$ClinicalSpecimenLinkage_DiseaseType))
# Summary table
demo_table <- 
  Global_data %>% select('Age At Diagnosis' = "AgeAtDiagnosis_1", "Sex", "Race", "Ethnicity", 'Disease Type' = "ClinicalSpecimenLinkage_DiseaseType") %>% 
  tbl_summary(missing = "no", sort = list(everything() ~ "frequency")) %>% 
  italicize_labels() %>% 
  as_gt
gt::gtsave(demo_table, expand = 1, zoom = 1, 
             paste0(
               path, 
               "/output data/Demographic table summary.pdf"))


####################################################################################################### II ### Treatment mining----
# Indicators
ind_col_names <- paste0(colnames(Indicators_v2), collapse = "|")
Indicators_v2 <- full_join(Indicators_v2, Follow_indicators_v2, by = "AvatarKey")
Indicators <- bind_rows(Indicators_v2, Indicators_v4, .id = "version") %>% 
  mutate(version = ifelse(version == 1, "v2", "v4")) %>% 
  # mutate(SmokingStatus = ifelse(version == "v2", "not recorded in v2", SmokingStatus)) %>% 
  mutate_at(c("SmokingStatus", "DVTDiagnosisInd", "HypercholesterolemiaDiagnosisInd",
            "HyperlipidemiaDiagnosisInd", "HeartDiseaseDiagnosisInd", "PulmonaryEmbolismDiagnosisInd"), 
            ~ ifelse(version == "v2", "not recorded in v2", .))
 
# List of all drugs
# drugs <- medication %>% select(Medication) %>% group_by(Medication) %>% mutate(n= n()) %>% distinct() %>% arrange(desc(n))
# write_csv(drugs, path = paste0(path, "/output data/Drugs/list of all drugs.csv"))
# medication %>% select(Medication) %>% tbl_summary(sort = list(everything() ~ "frequency"))

# Cardiotoxic drugs
cardiotox_drugs <- paste0(unique(cardiotox$Drug_names), collapse = "|")

drug_tox_patients <- medication[(grepl(cardiotox_drugs, medication$Medication)),] %>% 
  distinct(AvatarKey, Medication, AgeAtMedStart, AgeAtMedStop, .keep_all = TRUE) %>% 
  group_by(AvatarKey) %>% 
  mutate(count_total_times_cardiotoxic_drug = n()) %>% 
  select(c("AvatarKey", "Medication", "count_total_times_cardiotoxic_drug", AgeAtMedStart, AgeAtMedStop,
           "MedPrimaryDiagnosisSite")) %>% 
  ungroup() %>% 
  arrange(AvatarKey, AgeAtMedStart) %>% 
  left_join(. , mrn, by = "AvatarKey") %>% 
  left_join(., Indicators, by = "AvatarKey") %>% 
  left_join(., Age %>% select("MRN", "date_of_birth"), by = "MRN") %>% 
  # mutate(sec_birth = duration(AgeAtDeath, units = "years")) %>% 
  # mutate(calc_date_death = as.Date(date_of_birth) + sec_birth) %>% 
  mutate(across(starts_with("Age"), ~ as.numeric(.))) %>% 
  mutate(med_start_date = as.Date(date_of_birth) + ((AgeAtMedStart*365)+1)) %>% 
  mutate(med_stop_date = as.Date(date_of_birth) + ((AgeAtMedStop*365)+1)) %>% 
  select("AvatarKey", "MRN", "count_total_times_cardiotoxic_drug", "AgeAtMedStart","AgeAtMedStop", "Medication",  
         med_start_date, med_stop_date, everything(), -version)


write_csv(drug_tox_patients, paste0(path, "/output data/Drugs/patients receiving cardiotoxic drugs with indicators.csv"))

tbl <- drug_tox_patients %>% 
  distinct(AvatarKey, .keep_all = TRUE) %>%
  select("MedPrimaryDiagnosisSite", 
         "MedPrimaryDiagnosisSiteCode") %>% 
  tbl_summary() %>% bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/output data/Drugs/cancer summary of patients with cardiotoxic drugs.pdf"))

tbl <- drug_tox_patients %>% 
  distinct(AvatarKey, .keep_all = TRUE) %>%
  select("count_total_cardiotoxic_drug") %>% 
  tbl_summary() %>% bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/output data/Drugs/Drugs summary of patients with cardiotoxic drugs.pdf"))

tbl <- drug_tox_patients %>% 
  mutate(Whole = "count of drugs") %>% 
  select("Medication", Whole) %>% 
  tbl_summary(by = Whole) %>% bold_labels() %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*same drugs for a patients are counted*"))
gt::gtsave(tbl, zoom = 1, paste0(path, "/output data/Drugs/Drugs name summary of patients with cardiotoxic drugs.pdf"))


tbl <- drug_tox_patients %>% 
  distinct(AvatarKey, Medication, .keep_all = TRUE) %>%
  mutate(Whole = "count of drugs") %>% 
  select("Medication", Whole) %>% 
  tbl_summary(by = Whole) %>% bold_labels() %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*same drugs for a patients are counted only once*"))
gt::gtsave(tbl, zoom = 1, paste0(path, "/output data/Drugs/Drugs name summary of patients with cardiotoxic drugs2.pdf"))


tox_patient_ids <- paste0(drug_tox_patients$AvatarKey, collapse = "|")

tox_patient_data <- Vitals[(!grepl(tox_patient_ids, Vitals$AvatarKey)),] %>% 
  right_join(mrn, ., by = "AvatarKey") %>% 
  left_join(., Age, by = "MRN") %>% 
  # mutate(sec_birth = duration(AgeAtDeath, units = "years")) %>% 
  # mutate(calc_date_death = as.Date(date_of_birth) + sec_birth) %>% 
  mutate(days_calc_365 = AgeAtDeath*365) %>% 
  mutate(calc_date_death2 = as.Date(date_of_birth) + (days_calc_365)) # %>% 
  # mutate(days_calc_36525 = AgeAtDeath*365.25) %>% 
  # mutate(calc_date_death3 = as.Date(date_of_birth) + (days_calc_36525))
  

Vitals$AgeAtDeath_av2 <- as.integer(Vitals$AgeAtDeath_av)
mydatetime + lubridate::days(1)

tox_patient_data$calc_date_death = tox_patient_data$date_of_birth + dyears(tox_patient_data$AgeAtDeath_av)

tox_patient_data$calc_date_death = add_years(tox_patient_data$date_of_birth, (tox_patient_data$AgeAtDeath))

                                             
x <- year_month_day(2019, 1, 1)

add_years(x, 1:5)




                                             
                                             
                                             
                                             
                                             

