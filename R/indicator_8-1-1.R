# Indicator 8.1.1 ------------------------------------------------------
# Annual growth rate of real GDP per capita

library(dplyr)
library(cansim)
library(stringr)

real_gdp <- get_cansim("36-10-0222-01", factors = FALSE)
pop_ests <- get_cansim("17-10-0005-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

national_gdp <-
  real_gdp %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2014,
    Estimates == "Gross domestic product at market prices",
    Prices == "Chained (2017) dollars"
  ) %>% 
  select(
    REF_DATE,
    Geography = GEO,
    VALUE
  ) %>% 
  mutate(
    Year = substr(REF_DATE, 1, 4)
  ) %>% 
  group_by(Year, Geography) %>% 
  summarise(GDP = mean(VALUE), .groups = "drop")

pop_ests <- 
  pop_ests %>% 
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

all_gdp <- 
  bind_rows(
    national_gdp
  ) %>% 
  left_join(pop_ests) %>% 
  mutate(
    gdp_per_cap = (GDP*1000000)/Population
  ) %>% 
  arrange(Geography, Year) %>% 
  group_by(Geography) %>% 
  mutate(
    Geography = recode(Geography, Canada = ""),
    Value = (gdp_per_cap - lag(gdp_per_cap)) / lag(gdp_per_cap),
    Value = round(Value * 100, 2)
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

