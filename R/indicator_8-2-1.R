# Indicator 8.2.1 ------------------------------------------------------
# Annual growth rate of real GDP per employed person

library(dplyr)
library(cansim)
library(stringr)

real_gdp <- get_cansim("36-10-0222-01", factors = FALSE)
labour_force <- get_cansim("14-10-0327-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

exclude_Geo <- c(
  "Northwest Territories including Nunavut",
  "Outside Canada",
  "Yukon",
  "Nunavut",
  "Northwest Territories"
)

national_gdp <-
  real_gdp %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2014 & substr(REF_DATE, 1, 4) < substr(Sys.Date(), 1, 4),
    Estimates == "Gross domestic product at market prices",
    Prices == "Chained (2017) dollars",
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

labour_force_filtered <- 
  labour_force %>% 
  filter(
    REF_DATE >= 2014,
    `Labour force characteristics` == "Employment",
    Sex == "Both sexes",
    `Age group` == "15 years and over"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Employment = VALUE
  )

data_final <-
  left_join(national_gdp, labour_force_filtered) %>% 
  mutate(
    LabProd = (GDP*1000000)/(Employment)
  ) %>% 
  arrange(Geography, Year) %>% 
  group_by(Geography) %>% 
  transmute(
    Year, 
    Geography = recode(Geography, Canada = ""),
    # LabProd, year_before = lag(LabProd),
    Value = (LabProd - lag(LabProd)) / lag(LabProd),
    Value = round(Value * 100, 2)
  ) %>% 
  filter(Year > 2014) %>% 
  filter(
    !Geography %in% exclude_Geo
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = "Value")

write.csv(
  data_final,
  "data/indicator_8-2-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)  
