## GIF 3.b.1: Proportion of the target population covered by all vaccines included in their national programme

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

vaccine_coverage <- get_cansim("13-10-0870-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

data_final <-
  vaccine_coverage %>%
  filter(
    REF_DATE >= 2015,
    Characteristics == "Percentage vaccinated"
  ) %>%
  select(
    Year = REF_DATE,
    `Antigen or vaccine`,
    `Target population`,
    Geography = GEO,
    Gender = Sex,
    Value = VALUE
  ) %>%
  na.omit() %>%
  mutate(
    Gender = case_when(
      Gender == "Total - Gender" ~ "",
      TRUE ~ Gender
    )
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_3-b-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)