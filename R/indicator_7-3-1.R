# GIF 7.3.1 ---------------------------------------------------------------

options(scipen = 999)

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api

energy <- get_cansim("25-10-0029-01", factors = FALSE)
gdp <- get_cansim("36-10-0222-01", factors = FALSE)

filtered_energy <- 
  energy %>%
  filter(
    REF_DATE >= 2015,
    GEO != "Atlantic provinces",
    GEO != "Yukon, Northwest Territories and Nunavut",
    `Fuel type` == "Total primary and secondary energy",
    `Supply and demand characteristics`	 %in%  c("Energy use, final demand", "Total industrial", "Total transportation", "Agriculture, fishing, hunting and trapping", "Residential", "Public administration", "Commercial and other institutional")
  ) %>%
  select(
    Year = REF_DATE,
    energy_value = VALUE,
    Geography = GEO,
    `Supply and demand characteristics`
  )

filtered_gdp <- 
  gdp %>%
  filter(
    REF_DATE >= 2015,
    GEO != "Outside Canada",
    Prices == "Chained (2017) dollars",
    Estimates == "Gross domestic product at market prices"
  ) %>%
  select(
    Year = REF_DATE,
    gdp_value = VALUE,
    Geography = GEO
  )

data_final <-
  filtered_energy %>%
  left_join(filtered_gdp) %>%
  mutate(Value = round((energy_value / gdp_value), digits = 2)) %>%
  select(
    Year,
    Geography,
    `Supply and demand characteristics`,
    Value
  )

write.csv(
  data_final,
  "data/indicator_7-3-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

