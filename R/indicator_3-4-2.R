# Indicator 3.4.2 ---------------------------------------------------------
# Suicide mortality rate

library(cansim)
library(dplyr)
library(stringr)

suicide_mortality_data <- get_cansim("13-10-0800-01", factors = FALSE)

suicide_mort_rate <-
  suicide_mortality_data %>%
  filter(
    REF_DATE >= 2015,
    UOM == "Rate per 100,000 population",
    Characteristics == "Crude mortality rate per 100,000 population",
    `Cause of death (ICD-10)` == "Intentional self-harm (suicide) [X60-X84, Y87.0]"
  ) %>%
  mutate(GEO = str_remove(GEO, ", place of residence")) %>%
  select(Year = REF_DATE, Geography = GEO, Sex, Value = VALUE)

total_line <- 
  suicide_mort_rate %>%
  filter(Geography == "Canada" & Sex == "Both sexes") %>%
  mutate_at(c("Geography", "Sex"), ~ "")

data_final <-
  bind_rows(
    total_line,
    suicide_mort_rate %>%
      filter(!(Geography == "Canada" & Sex == "Both sexes"))
  ) %>% 
  filter(!str_starts(Geography, "Unknown"))

write.csv(
  data_final,
  "data/indicator_3-4-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
