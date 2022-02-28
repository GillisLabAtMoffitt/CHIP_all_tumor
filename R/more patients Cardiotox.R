# Find extra patients with WES from breast cancer sequential samples project

# import packages
library(tidyverse)

# I ### load data----

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "Breast CH")

breast_samples <- 
  readxl::read_xlsx(
    paste0(path, "/output/Sample lists/TCSPL-ServiceRequestForm_MCC 21545 Gillis 02.24.22.xlsx"),
    sheet = "Sample List")

treatment <- 
  read_rds(paste0(path, "/processed data/treatment_long.rds"))

# I ### List---
breast_samples <- 
  breast_samples %>% 
  mutate(mrn = as.character(MRN))
treatment <- treatment %>% 
  filter(str_detect(mrn, paste0(breast_samples$mrn, collapse = "|"))) %>% 
  ungroup()


# List of drugs for Jamila

write_csv(treatment %>% 
            filter(treatment_type != "radioT") %>% 
            select(treatment) %>% 
            distinct(),
          "List of drugs for Jamila.csv")
