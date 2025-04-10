# Indicator 10.5.1 ------------------------------------------------------
# 10.5.1 Financial Soundness Indicators

library(cansim)
library(dplyr)
library(stringr)

raw_data <- get_cansim("10-10-0146-01", factors = FALSE)

fsi <- raw_data %>%
  mutate(
    Year = substr(REF_DATE, 1, 4),
    Month = substr(REF_DATE, 6, 8),
  ) %>%
  filter(
    Year >= 2005,
    Month == "10", # use final quarterly value for annualized time series
  ) %>%
  select(
    Year,
    Indicator,
    Value = VALUE
  )

write.csv(fsi, "data/indicator_10-5-1.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8")  
