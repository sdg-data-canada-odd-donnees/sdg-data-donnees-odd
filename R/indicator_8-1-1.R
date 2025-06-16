# Indicator 8.1.1 ------------------------------------------------------
# Annual growth rate of real GDP per capita

library(dplyr)
library(cansim)
library(stringr)

real_gdp <- get_cansim("36-10-0222-01", factors = FALSE)
pop_ests <- get_cansim("17-10-0005-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

national_gdp <- real_gdp %>%
  filter(
    REF_DATE >= 2014,
    Estimates == "Gross domestic product at market prices",
    Prices == "Chained (2017) dollars"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    GDP = VALUE
  ) %>%
  arrange(Year, Geography) %>%
  na.omit()

pop_ests_filtered <- pop_ests %>%
  filter(
    REF_DATE >= 2014,
    Gender == "Total - gender",
    `Age group` == "All ages",
    UOM == "Persons"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Population = VALUE
  )

all_gdp <- national_gdp %>%
  inner_join(pop_ests_filtered) %>%
  mutate(
    gdp_per_cap = (GDP * 1000000) / Population
  ) %>%
  arrange(Geography, Year) %>%
  group_by(Geography) %>%
  mutate(
    Geography = recode(Geography, Canada = ""),
    Progress = round(gdp_per_cap, 2),
    Value = round(((gdp_per_cap - lag(gdp_per_cap)) / lag(gdp_per_cap)) * 100, 2)
  ) %>%
  filter(Year > 2014) %>%
  select(-c(3:5)) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = "Value")

write.csv(
  all_gdp,
  "data/indicator_8-1-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
