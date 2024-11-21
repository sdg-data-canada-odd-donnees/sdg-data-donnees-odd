# Indicator 11.3.1 ------------------------------------------------
# Proportion of population living below the national poverty line, by sex and age

library(dplyr)
library(readxl)
library(tidyr)
# library(stringr)

raw_data <- read_excel("~/StatisticsCanada_Data-SDG Indicator_11.3.1_July2024.xlsx", "2-Indicator 11.3.1 data inputs", range = "B8:S258")[-1,]

data <- raw_data[,c(1,9,10,14,16,18)] %>%
  # Rename columns
  rename_at(1, ~"Geography") %>%
  rename_at(2, ~"Population T2") %>%
  rename_at(3, ~"Population T3") %>%
  rename_at(4, ~"Land consumption rate") %>%
  rename_at(5, ~"Population growth rate") %>%
  rename_at(6, ~"Ratio of land consumption rate to population growth rate") %>%
  # Make columns numeric
  mutate_at(2:ncol(.), as.numeric) %>%
  # # Split province and city names into different columns
  # mutate(
  #   Province = str_extract(Geography, "^(.+), (.+)$", group = 2),
  #   City = str_extract(Geography, "^(.+), (.+)$", group = 1),
  # )
  # Filter out cities with population below 100,000
  filter(
    `Population T2` >= 100000 | `Population T3` >= 100000
  ) %>%
  select(
    Geography,
    `Ratio of land consumption rate to population growth rate`,
    `Land consumption rate`,
    `Population growth rate`
  ) %>%
  gather(key = "Series", value = "Value", -Geography) %>%
  # Add Year and Units columns
  mutate(
    Year = "2011-2021",
    Units = case_match(
      Series,
      "Ratio of land consumption rate to population growth rate" ~ "Ratio",
      .default = "Percentage (%)"
    ),
    Value = round(Value, 4)
  ) %>%
  relocate(Geography, .after = "Series") %>%
  relocate(Year, .before = "Series") %>%
  relocate(Units, .after = "Series")

write.csv(data, "data/indicator_11-3-1.csv", 
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
