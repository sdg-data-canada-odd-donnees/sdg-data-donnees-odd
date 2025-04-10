# Indicators 1.5.1, 11.5.1, and 13.1.1
library(dplyr)
library(cansim)
library(readr)
library(stringr)
library(tidyr)
library(hablar, include.only = "sum_")

# Canadian Disaster Database (downloaded as file named CDD.txt)
CDD_raw <- read_delim("CDD.txt", delim = "\t", show_col_types = FALSE)

# GDP CODR table
population_raw <- get_cansim("17-10-0005-01", factors = FALSE)

population <- population_raw %>%
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    Gender == "Total - gender",
    `Age group` == "All ages",
  ) %>%
  select(
    Year = REF_DATE,
    population = VALUE,
  )

disasters <- CDD_raw %>%
  mutate(
    Year = str_extract(`EVENT START DATE`, "\\d{4}"),
  ) %>%
  select(
    Year,
    "Type of natural disaster" = `EVENT TYPE`,
    Fatalities = FATALITIES,
    Injured_Infected = `INJURED / INFECTED`,
    Evacuated = EVACUATED,
  ) %>%
  mutate(
    number = if_else(
      is.na(Fatalities) & is.na(Injured_Infected) & is.na(Evacuated), 
      NA,
      rowSums(across(c(Fatalities, Injured_Infected, Evacuated)), na.rm = TRUE)
      )
  )

disasters_total <- disasters %>%
  summarise(Number = sum_(number, ignore_na = TRUE), .by = Year)

disasters_by_type <- disasters %>%
  summarise(Number = sum_(number, ignore_na = TRUE), .by = c(Year, "Type of natural disaster"))

final_data <- bind_rows(disasters_by_type, disasters_total) %>%
  left_join(population, by = "Year") %>%
  mutate("Rate per 100,000 population" = Number / population * 100000) %>%
  select(!population) %>%
  gather(key = "Units", value = "Value", -Year, -`Type of natural disaster`) %>%
  relocate(Units, .after = Year) %>%
  filter(!is.na(Value)) %>%
  arrange(desc(Units))

write.csv(final_data, "indicator_1-5-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
