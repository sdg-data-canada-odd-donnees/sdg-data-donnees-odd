## GIF 14.4.1

library(dplyr)
library(readr)
library(tidyr)

# Status of key fish stocks, Canada, 2011 to 2022

# Get data from source
# To fetch older or newer versions, change the year in the URL

national_data_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/sustainable-fish-harvest/2025/1_Fish-harvest-year%20.csv"

stocks_data_2025_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/sustainable-fish-harvest/2025/3_Fish-harvest-group.csv"
stocks_data_2024_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/sustainable-fish-harvest/2024/3_Fish-harvest-group.csv"
stocks_data_2023_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/sustainable-fish-harvest/2023/3_Fish-harvest-group.csv"
stocks_data_2022_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/sustainable-fish-harvest/2022/Fish-harvest-group.csv"

national_data <- read_csv(national_data_url, skip = 2, show_col_types = FALSE) %>% 
  na.omit()

stocks_data_2023 <- read_csv(stocks_data_2025_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group` = `Species group`,
         `At or below removal reference (number of stocks)`,
         `At or below other approved levels (number of stocks)`,
         `Above removal references or other approved levels (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2023")

stocks_data_2022 <- read_csv(stocks_data_2024_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group`,
         `At or below removal reference (number of stocks)`,
         `At or below other approved levels (number of stocks)`,
         `Above removal references or other approved levels (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2022")

stocks_data_2021 <- read_csv(stocks_data_2023_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group`,
         `At or below removal reference (number of stocks)`,
         `At or below other approved levels (number of stocks)`,
         `Above removal references or other approved levels (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2021")

stocks_data_2020 <- read_csv(stocks_data_2022_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group`,
         `At or below removal reference (number of stocks)`,
         `At or below other approved levels (number of stocks)`,
         `Above removal references or other approved levels (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2020")

fish_stocks <- national_data %>%
  # Tidy national data
  select(Year,
         `At or below removal reference` = `At or below removal reference (number of stocks)`,
         `At or below other approved levels` = `At or below other approved levels (number of stocks)`,
         `Above removal references or other approved levels` = `Above removal references or other approved levels (number of stocks)`
  ) %>%
  gather(key = "Stock group", value = "Number of stocks", -Year) %>%
  # Calculate percentages
  group_by(Year) %>%
  mutate(Percentage = `Number of stocks` / sum(`Number of stocks`) * 100) %>%
  select(
    Year,
    `Stock group`,
    Value = Percentage
  ) %>%
  filter(`Stock group` %in% c("At or below removal reference", "At or below other approved levels")) %>%
  ungroup() %>%
  summarise(Value = sum(Value), .by = c(Year)) %>%
  # blank out headline data
  mutate(`Stock group` = NA)

stock_groups <- bind_rows(stocks_data_2023, stocks_data_2022, stocks_data_2021, stocks_data_2020) %>%
  rename_at(vars(ends_with("(number of stocks)")), ~ substr(., 1, nchar(.)-19)) %>%
  filter(`Stock group` != "Total") %>%
  gather(key = "Status", value = "Number of stocks", -Year, -`Stock group`) %>%
  group_by(Year, `Stock group`) %>%
  mutate(Percentage = `Number of stocks` / sum(`Number of stocks`) * 100) %>%
  select(
    Year,
    `Stock group`,
    Status,
    Value = Percentage
  ) %>%
  filter(Status %in% c("At or below removal reference", "At or below other approved levels")) %>%
  ungroup() %>%
  summarise(Value = sum(Value), .by = c(Year, "Stock group"))

final_data <- bind_rows(fish_stocks, stock_groups) %>%
  relocate(`Stock group`, .before = Value)

# Write data to csv
write.csv(final_data, "data/indicator_14-4-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  