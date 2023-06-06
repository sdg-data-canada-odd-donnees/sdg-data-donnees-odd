# Indicator 10.4.1 ------------------------------------------------------
# 10.4.1 Labour share of GDP

library(cansim)
library(dplyr)
library(tidyr)

gdp <- get_cansim("36-10-0221-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

labour_share <- 
  gdp %>%
  filter(
    REF_DATE >= 2015,
    Estimates %in% c("Compensation of employees", "Gross domestic product at market prices")
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Estimates,
    Value = VALUE
  ) %>% 
  pivot_wider(
    names_from = Estimates,
    values_from = Value
  ) %>%
  transmute(
    Year, Geography,
    Value = round((`Compensation of employees`/`Gross domestic product at market prices`)*100, 2)
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = "Value")

data_final <- 
  bind_rows(
    labour_share %>%
      filter(Geography == "Canada") %>%
      mutate(across(2:(ncol(.)-2), ~ "")),
    labour_share %>%
      filter(Geography != "Canada")
  )

write.csv(
  data_final,
  "data/indicator_10-4-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)  
