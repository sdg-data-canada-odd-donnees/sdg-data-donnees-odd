# Indicator 6.4.1 ---------------------------------------------------------
# Change in water-use efficiency over time

library(dplyr)
library(readr)

# Source: https://sdg6data.org/en/tables
# Select Country = Canada, Indicator = Water availability and use > 6.4.1 Change in water-use efficiency over time
# Location of csv file downloaded from source
csvfile <- "~/../Downloads/sdg6data_download-12Nov2024.csv"

raw_data <- read_csv(csvfile)

wateruse_efficiency <- raw_data %>%
  filter(
    Year >= 2006,
    `Geographical area name` == "Canada",
  ) %>%
  select(
    Year,
    # Geography = `Geographical area name`,
    `Economic activity` = `SDG 6 Data portal level`,
    Value
  ) %>%
  mutate(
    Series = "Water-use efficiency (USD per cubic metre)",
    Units = "USD per cubic metre",
    `Economic activity` = sub(".*> ", "", `Economic activity`),
  ) %>%
  relocate(c(Series, Units), .after = Year)
  
percentchange <- wateruse_efficiency %>%
  group_by(`Economic activity`) %>%
  mutate(
    Series = "Percent change in water-use efficiency over time (%)",
    Units = "Percent change (%)",
    Value = (Value - lag(Value)) / lag(Value) * 100,
  )

data_final <- bind_rows(percentchange, wateruse_efficiency) %>%
  na.omit() %>%
  # set Overall as headline
  mutate(
    `Economic activity` = replace(`Economic activity`, `Economic activity` == "Overall", NA),
    Value = round(Value, 3)
  )

write.csv(data_final, "./data/indicator_6-4-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
