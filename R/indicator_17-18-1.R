# GIF 17.18.1 ---------------------------------------------------------------

# Unit of measure: Score
# Raw scores have values between 0 and 1 as recorded in the original assessment;
# subscores are simple totals of these raw scores. Standard scores are scaled from
# 0 to 100; subscores are weighted averages of these standard scores.

# Load libraries
library(purrr)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# --- Fetch SPI Index -------------------------------------------------------
spi_data <- read.csv("https://raw.githubusercontent.com/worldbank/SPI/master/03_output_data/SPI_index.csv")

SPI_index <- spi_data %>%
  filter(country == "Canada") %>%
  select(
    Year = date,
    `Pillar 1 - Data Use` = SPI.INDEX.PIL1,
    `Pillar 2 - Data Services` = SPI.INDEX.PIL2,
    `Pillar 3 - Data Products` = SPI.INDEX.PIL3,
    `Pillar 4 - Data Sources` = SPI.INDEX.PIL4,
    `Pillar 5 - Data Infrastructure` = SPI.INDEX.PIL5,
    SPI.INDEX
  ) %>%
  pivot_longer(-Year, names_to = "Pillar", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Pillar = ifelse(Pillar == "SPI.INDEX", NA, Pillar)) %>%
  arrange(desc(Year)) %>%
  mutate(Series = "Statistical Performance Indicators (SPI)") %>%
  relocate(Series, .before = "Pillar")


# --- Fetch ODIN datasets ---------------------------------------------------
years <- 2015:2030 # adjust range as needed
base_url <- "https://raw.githubusercontent.com/worldbank/SPI/master/01_raw_data/2.2_DSOA/ODIN_%d.csv"

dfs <- map(years, function(yr) {
  url <- sprintf(base_url, yr)
  tryCatch(
    {
      # Read all columns as character to avoid type mismatches across years
      df <- read_csv(url, show_col_types = FALSE, col_types = cols(.default = "c"))
      df$Year <- yr
      df
    },
    error = function(e) {
      warning(sprintf("Failed to fetch year %d: %s", yr, e$message))
      NULL
    }
  )
})

dfs_clean <- compact(dfs)

## This section is a hard-coded dataframe to append the latest ODIN data from
## https://odin.opendatawatch.com/Data/Download
## Update this section with the latest data whenever ODIN is updated

data_2024 <- data.frame(
  Year = c("2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024"),
  Region = c("North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America", "North America"),
  `Region code` = c("AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN", "AMN"),
  Country = c("Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada"),
  `Country Code` = c("CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN", "CAN"),
  `Data categories` = c("Population & vital statistics", "Education facilities", "Education outcomes", "Health facilities", "Health outcomes", "Reproductive health", "Food security & nutrition", "Gender statistics", "Crime & justice", "Poverty & income", "Social statistics subscore", "National accounts", "Labor", "Price indexes", "Government finance", "Money & banking", "International trade", "Balance of payments", "Digital Connectivity", "Economic & financial statistics subscore", "Agriculture & Land Use", "Resource use", "Energy", "Pollution", "Built environment", "Environment subscore", "All Categories"),
  `Indicator coverage and disaggregation` = c("100", "50", "50", "100", "100", "100", "50", "100", "100", "100", "85", "100", "100", "100", "100", "100", "100", "100", "-", "100", "100", "100", "100", "100", "50", "90", "91.7"),
  `Data available last 5 years` = c("100", "50", "50", "100", "100", "100", "50", "50", "100", "100", "80", "100", "100", "100", "100", "100", "100", "100", "-", "100", "50", "50", "100", "50", "50", "60", "80"),
  `Data available last 10 years` = c("100", "50", "50", "100", "100", "100", "50", "50", "100", "100", "80", "100", "100", "100", "100", "100", "100", "100", "-", "100", "50", "50", "100", "50", "50", "60", "80"),
  `First administrative level` = c("100", "50", "50", "50", "100", "50", "50", "100", "50", "100", "70", "0", "100", "50", "0", "-", "-", "-", "-", "37.5", "100", "100", "-", "-", "50", "83.3", "65.1"),
  `Second administrative level` = c("0", "0", "0", "100", "100", "0", "-", "0", "50", "100", "38.9", "-", "100", "-", "-", "-", "-", "-", "-", "100", "50", "-", "-", "-", "50", "50", "48"),
  `Coverage subscore` = c("80", "40", "40", "90", "100", "70", "50", "60", "80", "100", "71.4", "75", "100", "87.5", "75", "100", "100", "100", "-", "90.4", "70", "75", "100", "66.7", "50", "70", "76.6"),
  `Machine readability` = c("100", "100", "50", "100", "100", "100", "100", "50", "100", "100", "90", "100", "100", "100", "100", "100", "100", "100", "-", "100", "50", "100", "100", "0", "100", "70", "86.7"),
  `Non-proprietary` = c("100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "100", "-", "100", "100", "100", "100", "100", "100", "100", "100"),
  `Download options` = c("50", "50", "50", "50", "100", "100", "100", "50", "50", "100", "70", "100", "100", "100", "100", "100", "100", "100", "-", "100", "50", "50", "100", "50", "100", "70", "80"),
  `Metadata availability` = c("100", "100", "100", "50", "100", "100", "100", "50", "100", "100", "90", "100", "100", "100", "100", "50", "100", "100", "-", "92.9", "50", "50", "100", "50", "100", "70", "84.3"),
  `Terms of use` = c("50", "50", "50", "0", "50", "50", "50", "50", "50", "50", "45", "50", "50", "50", "50", "50", "50", "50", "-", "50", "50", "50", "50", "50", "50", "50", "48.3"),
  `Openness subscore` = c("80", "80", "70", "60", "90", "90", "90", "60", "80", "90", "79", "90", "90", "90", "90", "80", "90", "90", "-", "88.6", "60", "70", "90", "50", "90", "72", "79.9"),
  `Overall score` = c("80", "60", "55", "75", "95", "80", "72.2", "60", "80", "95", "75.2", "83.3", "95", "88.9", "83.3", "87.5", "93.8", "93.8", "-", "89.3", "65", "72.2", "93.8", "56.2", "70", "71.1", "78.3")
) %>%
  rename(
    `Region code` = Region.code,
    `Country Code` = Country.Code,
    `Data categories` = Data.categories,
    `Indicator coverage and disaggregation` = Indicator.coverage.and.disaggregation,
    `Data available last 5 years` = Data.available.last.5.years,
    `Data available last 10 years` = Data.available.last.10.years,
    `First administrative level` = First.administrative.level,
    `Second administrative level` = Second.administrative.level,
    `Coverage subscore` = Coverage.subscore,
    `Machine readable` = Machine.readability,
    `Non-proprietary` = Non.proprietary,
    `Download options` = Download.options,
    `Metadata available` = Metadata.availability,
    `Terms of use` = Terms.of.use,
    `Openness subscore` = Openness.subscore,
    `Overall score` = Overall.score
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

# Remove years that failed to load (returned NULL)
dfs_clean <- compact(dfs)

# Combine into one data frame, adding a year column if needed
odin_data <- bind_rows(dfs_clean, data_2024) %>%
  filter(
    Country == "Canada",
    !(Year %in% c("2021", "2023"))
  ) %>%
  select(
    -(Region),
    -(`Region code`),
    -(Country),
    -(`Country Code`)
  ) %>%
  gather(key = "Elements", value = "Value", -Year, -`Data categories`) %>%
  mutate(
    Value = as.numeric(Value),
    Series = "Open Data Inventory (ODIN)"
  ) %>%
  na.omit() %>%
  mutate(`Data categories` = ifelse(`Data categories` == "All Categories" & Elements == "Overall score", NA, `Data categories`)) %>%
  mutate(Elements = ifelse(`Data categories` == "All Categories" & Elements == "Overall score", NA, Elements)) %>%
  relocate(Series, .before = "Data categories") %>%
  relocate(Elements, .before = "Data categories") %>%
  mutate(
    `Data categories` = case_when(
      `Data categories` %in% c("Land use", "Agriculture & Land Use") ~ "Agriculture and land use",
      `Data categories` == "Poverty & income" ~ "Poverty and income",
      `Data categories` == "Population & vital statistics" ~ "Population and vital statistics",
      `Data categories` == "Money & banking" ~ "Money and banking",
      `Data categories` == "Economic & financial statistics subscore" ~ "Economic and financial statistics subscore",
      `Data categories` == "Crime & justice" ~ "Crime and justice",
      `Data categories` %in% c("Food security & nutritions", "Food security & nutrition") ~ "Food security and nutrition",
      `Data categories` == "All Categories" ~ "All categories",
      `Data categories` == "Energy" ~ "Energy use",
      TRUE ~ `Data categories`
    )
  ) %>%
  distinct()

data_final <- bind_rows(odin_data, SPI_index) %>%
  relocate(Pillar, .before = "Value")

# write data
write.csv(
  data_final,
  "data/indicator_17-18-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
