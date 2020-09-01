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
    AgeAtSpecimenCollection <= AgeAtSurgeryBiopsy_1 ~ "Yes",
    AgeAtSpecimenCollection > AgeAtSurgeryBiopsy_1 ~ "No"
  ))# %>% 
  # mutate(germ_before_bmt = case_when(
  #   AgeAtSpecimenCollection <= AgeAt ~ "Yes",
  #   AgeAtSpecimenCollection > AgeAt ~ "No"
  # ))