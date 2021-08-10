# import packages
library(tidyverse)
library(data.table)
library(lubridate)
library(gtsummary)

################################################################################################### I ### load data----

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                 "M2GEN")

mrn1 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/MRN.csv"), na = c("PRBB-DO NOT USE")) %>% 
  filter(!is.na(MRN))
mrn2 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/MRN.csv"), na = c("PRBB-DO NOT USE")) %>% 
  filter(!is.na(MRN))
mrn <- bind_rows(mrn1, mrn2) %>% 
  distinct()
rm(mrn1, mrn2)
# myfiles = list.files(path = paste0(path,
#   "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v2 <- lapply(myfiles, read_csv)

# myfiles <- list.files(path = paste0(path,
#   "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events"
# ), pattern = "*.csv", full.names = TRUE)
# 
# clinical_v4 <- lapply(myfiles, read_csv)
# V2----
Demo_v2 <- read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/Demographics.csv"))
Vitals_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/DemographicsVitalStatusLastContact.csv"))
Medication_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/Medications.csv"),
           col_types = cols(AgeAtMedStart = col_character(),
                            AgeAtMedStartFlag = col_character(),
                            AgeAtMedStopFlag = col_character(),
                            AgeAtMedStop = col_character()))
Radiation_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/Radiation.csv"))
Metastasis_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/DiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Metastasis_v2_f <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/FollowUpDiagnosisMetastaticDisease.csv")) %>% 
  select("AvatarKey", MetastaticDiseaseInd = "MetastaticDisease", 
         "MetastaticDiseaseSite", "MetastaticDiseaseSiteCode")
Staging_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/DiagnosisStaging.csv"), na = c("Not Available"),
           col_types = cols(AgeAtDiagnosisFlag = col_character(),
                            # AgeAtMedStartFlag = col_character(),
                            # AgeAtMedStopFlag = col_character(),
                            AgeAtPerformanceStatusAtDiagnosisFlag = col_character()))
Staging_v2_f <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/FollowUpDiagnosisStaging.csv"),
           col_types = cols(YearOfDiagnosis = col_double()))
Surgery_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/SurgeryBiopsy.csv"),
           col_types = cols(AgeAtSurgeryBiopsyFlag = col_character(),
                            AgeAtSurgeryBiopsy = col_character()))
TMarkers_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/TumorMarkers.csv"),
           col_types = cols(AgeAtTumorMarkerTestFlag = col_character()))
Sequencing_v2 <- 
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/ClinicalSpecimen.csv")) %>% 
  select("AvatarKey", "ORIENSpecimenID", "WES", "AgeAtSpecimenCollection", "Tumor/Germline")

Indicators_v2 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/Indicators.csv")) %>% 
  select("AvatarKey", "COPDDiagnosisInd", "CVAStrokeDiagnosisInd", "InsulinDependentDiabetesMellitusDiagnosisInd", 
         "HypertensionDiagnosisInd", "MIHeartFailureDiagnosisInd", "VenousThrombosisDrugToxicityInd") %>% 
  distinct()
Follow_indicators_v2 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v2_clinical-with-events/FollowUpIndicators.csv")) %>% 
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

cardiotox <- 
  readxl::read_xlsx(paste0(path, "/Jamila Mammadova/data/Cardiotoxic drugs Jamila.xlsx"), na = "NA", n_max = 53)

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

sample_data <- 
  readxl::read_xlsx(paste0(path, "/Yifen data/Avatar SLIDs and 06Sdatesv2.xlsx"),
                    na = "NULL") %>% 
  select(-DateOfCollection,
         tumor_anatomic_site = "tumor anatomic site", germline_anatomic_site = "germline anatomic site")

# Jamila
cardiot_patients <- 
  readxl::read_xlsx(paste0(path, "/Jamila Mammadova/data/Cardio events RFs_Jamila_v2.xlsx"),
                    sheet = "Restructured_DCS", skip = 2, n_max = 150) %>% 
  select(-SN)

drugs_date <- tibble::tribble(
   ~SN,     ~MRN,   ~D_TX_IN, ~D_CTOX_IN,
    1L,  522580L, "11/28/07", "11/28/07",
    2L,  537621L, "05/09/08", "05/09/08",
    3L,  195371L, "07/29/08", "07/29/08",
    4L,  558707L, "05/08/09", "05/08/09",
    5L,  333866L, "12/07/06", "12/27/06",
    6L,  524227L, "08/15/07", "08/15/07",
    7L,  530325L, "10/30/07", "10/30/07",
    8L,  550010L, "02/19/09", "02/19/09",
    9L,  572238L, "01/12/10", "01/12/10",
   10L,  584576L, "05/20/10", "05/20/10",
   11L,  589302L, "10/12/10", "10/12/10",
   12L,  648002L, "05/02/13", "05/02/13",
   13L,  647730L, "05/07/13", "05/07/13",
   14L,  655058L, "09/13/13", "09/13/13",
   15L,  661464L, "03/17/14", "03/17/14",
   16L,  655247L, "08/21/13", "08/21/13",
   17L,  732895L, "01/10/17", "01/10/17",
   18L,  758760L, "11/10/17", "11/10/17",
   19L,  536929L, "04/16/18", "05/29/18",
   20L,  766918L, "01/17/18", "01/17/18",
   21L,  800770L, "09/18/18", "09/18/18",
   22L,  502272L, "07/13/18", "07/13/18",
   23L,  306041L, "04/01/03", "12/01/04",
   24L,  537019L, "03/28/11", "04/01/18",
   25L,  670404L, "9/9/2014", "07/14/15",
   26L,  236494L, "10/07/08", "10/07/08",
   27L,  546808L, "10/14/08", "10/14/08",
   28L,  570178L, "09/16/09", "09/16/09",
   29L,  573880L, "12/18/09", "12/18/09",
   30L,  579905L, "03/16/10", "03/16/10",
   31L,  579081L, "03/15/10", "03/15/10",
   32L,  582032L, "06/03/10", "06/03/10",
   33L,  590361L, "08/24/10", "08/24/10",
   34L,  671165L, "04/05/14", "04/08/14",
   35L,  265133L, "01/31/03", "01/31/03",
   36L,  549646L, "09/15/08", "09/18/08",
   37L,  287169L, "10/31/05", "01/06/07",
   38L,  275988L, "06/09/03", "06/09/03",
   39L,  318193L, "09/01/07", "12/29/08",
   40L,  301139L, "01/17/05", "01/17/05",
   41L,  302415L, "02/15/05", "02/15/05",
   42L,  569237L, "08/10/09", "08/10/09",
   43L,  600265L, "02/28/11", "05/09/14",
   44L,  643086L, "09/01/12", "02/07/14",
   45L,  591531L, "09/10/10", "10/16/15",
   46L,  511525L, "05/07/11", "07/26/13",
   47L,  323746L, "06/08/11", "03/10/15",
   48L,  659753L, "11/18/13", "03/09/16",
   49L,  323480L, "05/16/06", "05/16/06",
   50L,  328395L, "01/10/07", "01/10/07",
   51L,  331159L, "11/29/06", "11/29/06",
   52L,  525586L, "11/08/07", "01/03/08",
   53L,  533035L, "04/22/08", "05/06/08",
   54L,  187151L, "03/11/09", "03/11/09",
   55L,  497168L, "05/27/09", "05/27/09",
   56L,  278476L, "01/06/10", "01/06/10",
   57L,  573601L, "11/16/09", "11/16/09",
   58L,  568476L, "07/27/09", "07/27/09",
   59L,  581183L, "04/16/10", "04/16/10",
   60L,  586051L, "07/13/10", "07/13/10",
   61L,  592017L, "11/05/10", "11/05/10",
   62L,  602898L, "05/02/11", "05/02/11",
   63L,  606552L, "07/06/11", "07/06/11",
   64L,  608504L, "08/29/11", "11/30/11",
   65L,  615075L, "12/02/11", "12/02/11",
   66L,  616542L, "12/27/11", "12/27/11",
   67L,  621827L, "01/22/16", "01/22/16",
   68L,  641808L, "02/06/13", "02/06/13",
   69L,  642719L, "02/18/13", "02/18/13",
   70L,  643509L, "02/25/13", "02/25/13",
   71L,  655881L, "10/16/13", "10/16/13",
   72L,  670349L, "06/10/14", "06/10/14",
   73L,  679276L, "12/03/14", "12/03/14",
   74L,  699578L, "08/13/15", "08/13/15",
   75L,  708288L, "12/09/15", "12/09/15",
   76L,  708025L, "01/20/16", "01/20/16",
   77L,  706967L, "03/10/16", "03/10/16",
   78L,  298270L, "06/29/07", "06/29/07",
   79L,  532627L, "04/01/08", "04/01/08",
   80L,  551902L, "07/22/13", "11/18/16",
   81L,  554521L, "02/01/11", "02/01/11",
   82L,  230121L, "05/20/14", "05/20/14",
   83L,  601457L, "02/22/11", "02/22/11",
   84L,  606659L, "05/18/11", "05/18/11",
   85L,  612672L, "09/14/11", "09/14/11",
   86L,  612471L, "09/09/11", "09/09/11",
   87L,  629560L, "09/18/15", "09/18/15",
   88L,  646438L, "04/29/13", "07/01/15",
   89L,  672758L, "01/28/16", "01/28/16",
   90L,  650845L, "10/07/15", "10/07/15",
   91L,  656908L, "05/14/15", "05/14/15",
   92L,  644788L, "03/08/14", "03/08/14",
   93L,  659277L, "10/02/13", "10/02/13",
   94L,  670813L, "04/25/14", "04/25/14",
   95L,  681791L, "11/09/14", "11/09/14",
   96L,  690359L, "03/10/17", "03/10/17",
   97L,  629361L, "05/16/14", "05/16/14",
   98L,  465302L, "02/16/17", "03/04/17",
   99L,  705307L, "10/17/15", "10/17/15",
  100L,  718871L, "04/13/16", "04/13/16",
  101L,  608501L, "05/16/11", "11/17/12",
  102L,  520148L, "07/08/16", "07/08/16",
  103L,  334037L, "03/02/07", "03/05/07",
  104L,  527740L, "01/11/08", "01/11/08",
  105L,  291010L, "04/12/11", "04/12/11",
  106L,  668105L, "03/10/14", "03/13/14",
  107L,  670416L, "05/07/14", "05/07/14",
  108L,  727076L, "01/09/17", "01/09/17",
  109L,  675420L, "07/11/14", "07/11/14",
  110L,  755766L, "08/22/17", "09/15/17",
  111L,  627985L, "05/02/12", "05/02/12",
  112L,  649967L, "11/19/13", "05/09/14",
  113L,  654471L, "06/28/13", "06/28/13",
  114L,  654787L, "07/01/13", "06/09/15",
  115L,  658873L, "02/04/15", "02/04/15",
  116L,  567315L, "07/01/09", "07/21/10",
  117L,  695873L, "06/03/15", "11/25/16",
  118L,  762964L, "07/01/16", "01/04/18",
  119L,  636291L, "08/31/12", "08/31/12",
  120L,  693031L, "04/21/15", "04/21/15",
  121L,  728049L, "12/02/16", "06/08/18",
  122L,  767527L, "02/28/18", "03/07/18",
  123L,  636060L, "08/14/12", "08/14/12",
  124L,  647362L, "04/26/13", "05/09/13",
  125L,  644270L, "01/18/13", "01/18/13",
  126L,  710310L, "11/01/16", "11/01/16",
  127L,  722340L, "06/14/16", "06/14/16",
  128L,  732970L, "11/15/16", "08/13/18",
  129L,  734045L, "01/20/17", "01/20/17",
  130L,  739551L, "02/22/17", "02/22/17",
  131L,  743363L, "05/16/17", "05/16/17",
  132L,  745539L, "07/12/17", "07/12/17",
  133L,  763829L, "01/24/18", "01/24/18",
  134L,  771550L, "03/15/18", "03/15/18",
  135L,  700461L, "10/13/17", "10/13/17",
  136L,  609049L, "04/18/18", "04/18/18",
  137L, 1010046L, "06/22/18", "06/22/18",
  138L,  801211L, "09/10/18", "09/10/18",
  139L,  641591L, "06/13/14", "10/25/14",
  140L,  562119L, "05/11/09", "05/14/09",
  141L,  625387L, "11/28/11", "02/25/13",
  142L,  537214L, "05/22/08", "05/22/08",
  143L,  754799L, "03/01/17", "11/03/17"
  ) %>% 
  select(-SN) %>% 
  mutate(across(where(is.character), ~ as.POSIXct(., format = "%m/%d/%y", tz = "UTC"))) %>% 
  rename(D_TX_IN_jamila = D_TX_IN)

drugs_date <- 
  readxl::read_xlsx(paste0(path, "/Jamila Mammadova/data/Data_For_Christelle.xlsx")) %>% 
  select(-SN) %>% 
  rename(D_TX_IN_jamila = D_TX_IN)
  
cardiot_patients <- cardiot_patients %>% full_join(., drugs_date, by = "MRN")



################################################################################################### II ### Data cleaning----
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

rm(demo, Demo_v2, Demo_v4)  
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
  mutate(interval_germ_tumor = abs(interval(start= DateOfCollection_germline, end= tunor_collection_dt)/duration(n=1, unit="days"))) %>% 
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
  select(-n, -is_tumor_closest_to_germline)

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









# Vitals
Vitals_v2$AvatarKey[which(duplicated(Vitals_v2$AvatarKey))]

## PROBLEM : Found patient with 2 age at date, ex: A007107 ##
# Vitals_v2_test <- Vitals_v2 %>%
#   group_by(AvatarKey) %>% 
#   mutate(death_max = max(AgeAtDeath))
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
