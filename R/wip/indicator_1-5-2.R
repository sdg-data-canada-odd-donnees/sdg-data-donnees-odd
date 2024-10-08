# Indicators 1.5.2 and 11.5.2
library(dplyr)
library(cansim)
library(readr)
library(stringr)
library(hablar, include.only = "sum_")

# Canadian Disaster Database (downloaded as file named CDD.txt)
CDD_raw <- read_delim("CDD.txt", delim = "\t", show_col_types = FALSE)

# GDP CODR table
gdp_raw <- get_cansim("36-10-0222-01", factors = FALSE)

gdp <- gdp_raw %>%
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    Prices == "Current prices",
    Estimates == "Gross domestic product at market prices",
  ) %>%
  select(
    Year = REF_DATE,
    GDP = VALUE,
  )

disasters <- CDD_raw %>%
  mutate(Year = str_extract(`EVENT START DATE`, "\\d{4}")) %>%
  select(
    Year,
    "Type of natural disaster" = `EVENT TYPE`,
    Value = `ESTIMATED TOTAL COST`
  )

disasters_total <- disasters %>%
  summarise(Value = sum_(Value, ignore_na = TRUE), .by = Year)

disasters_by_type <- disasters %>%
  summarise(Value = sum_(Value, ignore_na = TRUE), .by = c(Year, "Type of natural disaster"))

final_data <- bind_rows(disasters_by_type, disasters_total) %>%
  left_join(gdp, by = "Year") %>%
  mutate(Value = 100 * Value / GDP / 1000000) %>%
  select(Year, "Type of natural disaster", Value) %>%
  filter(!is.na(Value))

write.csv(final_data, "indicator_1-5-2.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")

