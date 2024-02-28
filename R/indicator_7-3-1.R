# GIF 7.2.1 ---------------------------------------------------------------

options(scipen = 999)

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api

energy <- get_cansim("38-10-0096-01", factors = FALSE)
gdp <- get_cansim("36-10-0222-01", factors = FALSE)

filtered_energy <- 
  energy %>%
  filter(
    REF_DATE >= 2015,
    Sector == "Total, industries and households"
  ) %>%
  select(
    Year = REF_DATE,
    energy_value = VALUE
  )

filtered_gdp <- 
  gdp %>%
  filter(
    REF_DATE >= 2015,
    Prices == "Chained (2017) dollars",
    Estimates == "Gross domestic product at market prices",
    GEO == "Canada"
  ) %>%
  select(
    Year = REF_DATE,
    gdp_value = VALUE
  )

data_final <-
  filtered_energy %>%
  left_join(filtered_gdp) %>%
  mutate(Value = round((energy_value / gdp_value), digits = 8)) %>%
  select(
    Year,
    Value
  )

write.csv(
  data_final,
  "data/indicator_7-3-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

