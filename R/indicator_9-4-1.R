# 9.4.1 ------------------------------------------------------------------
# CO2 emission per unit of value added

library(dplyr)
library(readr)

CO2_per_GDP_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/ghg-emissions/2024/ghg-emissions-intensity-en.csv"

CO2_per_GDP <- read_csv(CO2_per_GDP_url, skip = 2, show_col_types = FALSE) %>%
  na.omit() %>%
  select(
    Year,
    Value = "Greenhouse gas emissions per unit of gross domestic product (megatonnes of carbon dioxide equivalent per billion dollars gross domestic product)"
  )

write.csv(CO2_per_GDP, "data/indicator_9-4-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
