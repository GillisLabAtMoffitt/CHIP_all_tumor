############# Read the files in 01.data loading


#############  Clean clinical v4.7

# Cardiotoxicities
cardiotox <- cardiotox %>% 
  `colnames<-`(str_replace_all(colnames(.), " ", "_")) %>% 
  fill(Chemoterapeutical_drug, Incidence_rate, Cardiovascular_manifestations, .direction = "down") %>% 
  mutate(Drug_names = tolower(Drug_names),
         Drug_names = str_replace_all(Drug_names, ", and | and |, ", "|"), 
         
         Drug_names = str_remove(Drug_names, "5-|.*: "), 
         Drug_names = gsub("\\(.*?)", "", Drug_names),
         Drug_names = str_replace_all(Drug_names, "interferon-alfa", "interferon alfa")) 

# Cardiotoxic drugs
cardiotox_drugs <- paste0(unique(cardiotox$Drug_names), collapse = "|")

drug_tox <- Medication_v4_7 %>% 
  mutate(medication = str_to_lower(medication)) %>% 
  # Filter for the cardiotoxic drugs
  filter(str_detect(medication, cardiotox_drugs)) %>% 
  distinct(avatar_key, medication, age_at_med_start, age_at_med_stop, .keep_all = TRUE) %>%
  mutate(age_at_med_start = case_when(
    age_at_med_start == "Age 90 or older"            ~ 90,
    age_at_med_start == "Unknown/Not Applicable"     ~ NA_real_,
    TRUE                                             ~ as.numeric(age_at_med_start)
  ), age_at_med_start = as.numeric(age_at_med_start)) %>% 
  group_by(avatar_key) %>%
  mutate(count_total_times_cardiotoxic_drug = n()) %>%
  ungroup() 

drug_tox_patients <- drug_tox %>% 
  # Select patient treatments who have samples 
  inner_join(. , sample_data_v4_7, by = "avatar_key") %>% 
  # # Need dob
  # left_join(., Diagnosis_v4_7a %>% 
  #             select(avatar_key, dob),
  #           by = "avatar_key") %>% 
  # calculate drug_start_date
  mutate(drug_start_date = as.Date(dob) + (age_at_med_start * 365)
           ) %>% 
  mutate(sample_bf_drugs = case_when(
    date_of_specimen_collection < drug_start_date         ~ "Yes",
    TRUE                                                  ~ "No"
  )) %>% 
  # sample needs to be before the very first toxic drug --------------here consider ALL the types of drugs
  arrange(avatar_key, drug_start_date) %>% 
  distinct(avatar_key, .keep_all = TRUE) %>% 
  filter(sample_bf_drugs == "Yes")
  
  
  
drug_tox_patients1 <- drug_tox_patients %>% 
  mutate(mrn = as.character(mrn)) %>% 
  select(avatar_key, mrn, everything(), 
         -orien_specimen_id, -dna_sequencing_library_id, -sample_family_id) %>% 
  # Remove the ids already checked in previous version
  filter(!str_detect(avatar_key, previous_list)) %>%
  filter(!str_detect(mrn, paste0(previous_list1$mrn, collapse = "|"))) %>%
  mutate(patient = dense_rank(avatar_key), .before = 1) 

write_csv(drug_tox_patients1, "Patients receiving cardiotoxic drugs in v4_6and7 07082022.csv")

