# 11.1.1 ------------------------------------------------------------------
# Proportion of urban population living in slums, informal settlements or inadequate housing

library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

raw_data <- get_cansim("46-10-0085-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

core_housing <- raw_data %>%
  filter(
    `Core housing need statistics` == "Percentage of persons in core housing need",
    str_detect(GEO, "urban")
  ) %>% 
  select(
    Year = REF_DATE,
    Tenure = `Tenure including first-time homebuyer and social and affordable housing status`,
    Geography = GEO,
    Value = VALUE
  ) %>%
  mutate(
    Geography = str_remove(Geography, "Large urban population centres, "),
    Geography = case_when(
      Geography == "Total, large urban population centres" ~ "Canada",
      TRUE ~ Geography
    )) %>%
  mutate(
    # Set headline
    across(
      c(Geography, Tenure),
      ~ replace(., Geography == "Canada" & Tenure == "Total, tenure", NA)
    )
  ) %>%
  drop_na(Value) %>%
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = "Value")

write.csv(
  core_housing,
  "data/indicator_11-1-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
