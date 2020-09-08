################################################################################################### I ### Germline comparaison----
Global_data <- Global_data %>% 
  mutate(germ_before_drugs = case_when(
    AgeAtSpecimenCollection <= AgeAtMedStart_regimen1 ~ "Yes",
    AgeAtSpecimenCollection > AgeAtMedStart_regimen1 ~ "No"
  )) %>% 
  mutate(germ_before_rad = case_when(
    AgeAtSpecimenCollection <= AgeAtRadiationStart_regimen1 ~ "Yes",
    AgeAtSpecimenCollection > AgeAtRadiationStart_regimen1 ~ "No"
  )) %>% 
  mutate(germ_before_surgery = case_when(
    AgeAtSpecimenCollection <= AgeAtSurgeryBiopsy_regimen1 ~ "Yes",
    AgeAtSpecimenCollection > AgeAtSurgeryBiopsy_regimen1 ~ "No"
  )) %>% 
  mutate(germ_before_bmt = case_when(
    AgeAtSpecimenCollection <= AgeAtTransplant_1 ~ "Yes",
    AgeAtSpecimenCollection > AgeAtTransplant_1 ~ "No"
  ))

# write_csv(Global_data, paste0(path, "/output data/cleaned files/Global_data.csv"))


################################################################################################### II ### Lists----

germ_BF_drugs <- Global_data %>%
  filter(germ_before_drugs == "Yes") %>% 
  select(c("AvatarKey", "Histology_1", "ORIENSpecimenID", "PrimaryDiagnosisSite_1", "AgeAtSpecimenCollection", 
           "AgeAtMedStart_regimen1", "AgeAtTransplant_1", "AgeAtRadiationStart_regimen1",
           "AgeAtSurgeryBiopsy_regimen1")) %>% 
  arrange(Histology_1)
write.csv(germ_BF_drugs, paste0(path, "/output data/List patients for sequencing/List patients germline before drugs.csv"))

germ_BF_rad <- Global_data %>%
  filter(germ_before_rad == "Yes") %>% 
  select(c("AvatarKey", "Histology_1", "ORIENSpecimenID", "PrimaryDiagnosisSite_1", "AgeAtSpecimenCollection", 
           "AgeAtMedStart_regimen1", "AgeAtTransplant_1", "AgeAtRadiationStart_regimen1",
           "AgeAtSurgeryBiopsy_regimen1")) %>% 
  arrange(Histology_1)
write.csv(germ_BF_rad, paste0(path, "/output data/List patients for sequencing/List patients germline before radiation.csv"))

germ_before_surgery <- Global_data %>%
  filter(germ_before_surgery == "Yes") %>% 
  select(c("AvatarKey", "Histology_1", "ORIENSpecimenID", "PrimaryDiagnosisSite_1", "AgeAtSpecimenCollection", 
           "AgeAtMedStart_regimen1", "AgeAtTransplant_1", "AgeAtRadiationStart_regimen1",
           "AgeAtSurgeryBiopsy_regimen1")) %>% 
  arrange(Histology_1)
write.csv(germ_before_surgery, paste0(path, "/output data/List patients for sequencing/List patients germline before surgery"))

germ_before_bmt <- Global_data %>%
  filter(germ_before_bmt == "Yes") %>% 
  select(c("AvatarKey", "Histology_1", "ORIENSpecimenID", "PrimaryDiagnosisSite_1", "AgeAtSpecimenCollection", 
           "AgeAtMedStart_regimen1", "AgeAtTransplant_1", "AgeAtRadiationStart_regimen1",
           "AgeAtSurgeryBiopsy_regimen1")) %>% 
  arrange(Histology_1)
write.csv(germ_before_bmt, paste0(path, "/output data/List patients for sequencing/List patients germline before bmt"))

germ_before_drugs_bmt <- Global_data %>%
  filter(germ_before_drugs == "Yes") %>% 
  filter(germ_before_bmt == "Yes") %>% 
  select(c("AvatarKey", "Histology_1", "ORIENSpecimenID", "PrimaryDiagnosisSite_1", "AgeAtSpecimenCollection", 
           "AgeAtMedStart_regimen1", "AgeAtTransplant_1", "AgeAtRadiationStart_regimen1",
           "AgeAtSurgeryBiopsy_regimen1")) %>% 
  arrange(Histology_1)
write.csv(germ_before_drugs_bmt, paste0(path, "/output data/List patients for sequencing/List patients germline before drugs and  bmt"))







