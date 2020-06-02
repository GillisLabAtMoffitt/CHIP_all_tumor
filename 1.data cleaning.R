# import packages
library(tidyverse)

# load data

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                 "M2GEN")
myfiles = list.files(path = paste0(path,
  "/Garrick/10R20000134_2020-05-05_avatar_v2_clinical-with-events"
), pattern = "*.csv", full.names = TRUE)

clinical_v2 <- lapply(myfiles, read_csv)
#---------------------------------------------------------------------------------------------------
myfiles <- list.files(path = paste0(path,
  "/Garrick/10R20000134_2020-05-05_avatar_v4_clinical-with-events"
), pattern = "*.csv", full.names = TRUE)

clinical_v4 <- lapply(myfiles, read_csv)
#---------------------------------------------------------------------------------------------------
