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

national_gdp <- real_gdp %>%
  filter(
    REF_DATE >= 2014,
    Estimates == "Gross domestic product at market prices",
    Prices == "Chained (2017) dollars",
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    GDP = VALUE
  ) %>%
  arrange(Year, Geography) %>%
  na.omit()

labour_force_filtered <- labour_force %>%
  filter(
    REF_DATE >= 2014,
    `Labour force characteristics` == "Employment",
    Gender == "Total - Gender",
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
    LabProd = (GDP * 1000000) / (Employment)
  ) %>%
  arrange(Geography, Year) %>%
  group_by(Geography) %>%
  transmute(
    Year,
    Geography = recode(Geography, Canada = ""),
    Progress = round(LabProd, 2),
    # LabProd, year_before = lag(LabProd)
    Value = round(((LabProd - lag(LabProd)) / lag(LabProd)) * 100, 2)
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
