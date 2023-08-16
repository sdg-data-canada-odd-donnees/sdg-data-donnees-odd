
# Indicator 15.a.1.b ------------------------------------------------------
# Public expenditures on conservation and sustainable use of biodiversity and ecosystems

library(cansim)
library(here)
library(dplyr)
library(stringr)
library(readr)

public_expenditure <- get_cansim("10-10-0005-01", factors = FALSE)

geocodes <- read_csv("gif-data-processing/geocodes.csv")

protection_biodiversity <- 
  public_expenditure %>%
  filter(
    str_detect(`Canadian Classification of Functions of Government (CCOFOG)`, "Protection of biodiversity and landscape"),
    REF_DATE >= 2015
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Public sector components`,
    Value = VALUE
  ) %>%
  mutate(
    Value = Value * 1000000
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    protection_biodiversity %>%
      filter(Geography == "Canada", `Public sector components` == "Consolidated Canadian general government") %>%
      mutate(across(Geography: `Public sector components`, ~ "")),
    protection_biodiversity %>%
      filter(!(Geography == "Canada" & `Public sector components` == "Consolidated Canadian general government"))
  )

write_csv(data_final, here("gif-data-processing", "data", "indicator_15-a-1.csv"), na = "")

