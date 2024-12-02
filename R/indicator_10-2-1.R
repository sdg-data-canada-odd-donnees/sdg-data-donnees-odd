# Indicator 10.2.1 ------------------------------------------------------
# 10.2.1 Proportion of people living below 50 percent of median income, by sex, age and persons with disabilities

library(cansim)
library(dplyr)
library(stringr)

low_income_data <- get_cansim("11-10-0135-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

persons_in_low_income <- 
  low_income_data %>%
  filter(
    REF_DATE >= 2015,
    `Low income lines` == "Low income measure after tax",
    Statistics == "Percentage of persons in low income",
    !str_starts(`Hierarchy for Persons in low income`, "13"),
    !str_starts(`Hierarchy for Persons in low income`, "26")
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Persons in low income`, 
    #"Hierarchy for Persons in low income",
    Value = VALUE
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = Value) %>%
  mutate(
    GeoCode = ifelse(`Persons in low income` == "All persons", GeoCode, NA)
    )

data_final <- 
  bind_rows(
    persons_in_low_income %>%
      filter(Geography == "Canada", `Persons in low income` == "All persons") %>%
      mutate(across(Geography: `Persons in low income`, ~ NA)),
    persons_in_low_income %>%
      filter(!(Geography == "Canada" & `Persons in low income` == "All persons"))
  )

write.csv(data_final, "data/indicator_10-2-1.csv", 
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
