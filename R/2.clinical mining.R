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
# List of all drugs
drugs <- medication %>% select(Medication) %>% group_by(Medication) %>% mutate(n= n()) %>% distinct() %>% arrange(desc(n))
write_csv(drugs, path = paste0(path, "/output data/Drugs/list of all drugs.csv"))
# medication %>% select(Medication) %>% tbl_summary(sort = list(everything() ~ "frequency"))












