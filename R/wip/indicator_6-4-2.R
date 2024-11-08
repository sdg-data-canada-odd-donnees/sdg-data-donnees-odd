# Indicator 6.4.2 ---------------------------------------------------------
# Level of water stress: freshwater withdrawal as a proportion of available freshwater resources

library(dplyr)
library(readr)

# Source: https://sdg6data.org/en/tables
# Select Country = Canada, Indicator = Water availability and use > 6.4.2 Level of water stress: freshwater withdrawal as a proportion of available freshwater resources

# Location of csv file downloaded from source
csvfile <- "~/../Downloads/sdg6data_download-8Nov2024.csv"

raw_data <- read_csv(csvfile)

waterstress <- raw_data %>%
  filter(
    Year >= 2000,
    `Geographical area name` == "Canada",
  ) %>%
  select(
    Year,
    # Geography = `Geographical area name`,
    `Economic activity` = `SDG 6 Data portal level`,
    Value
  ) %>%
  mutate(`Economic activity` = sub(".*> ", "", `Economic activity`)) %>%
  # filter out Total (duplicate of Overall)
  filter(`Economic activity` != "Total") %>%
  # set Overall as headline
  mutate(
    `Economic activity` = replace(`Economic activity`, `Economic activity` == "Overall (%)", NA)
  )

write.csv(waterstress, "./data/indicator_6-4-2.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
