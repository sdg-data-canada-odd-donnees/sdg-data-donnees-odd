## GIF 3.b.1: Proportion of the target population covered by all vaccines included in their national programme

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

vaccine_coverage <- get_cansim("13-10-0870-01", factors = FALSE)

data_final <-
  vaccine_coverage %>%
  filter(
    REF_DATE >= 2015,
    Characteristics == "Percentage vaccinated",
    `Target population` != "Recommended vaccines during pregnancy",
  ) %>%
  select(
    Year = REF_DATE,
    `Antigen or vaccine`,
    `Target population`,
    Geography = GEO,
    Gender = Sex,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  na.omit() %>%
  mutate(
    GeoCode = as.integer(GeoCode)
  )

write.csv(
  data_final,
  "data/indicator_3-b-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)