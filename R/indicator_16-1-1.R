# Indicator 16.1.1 ---------------------------------------------------------
#  Number of victims of intentional homicide per 100,000 population, by sex and age

library(cansim)
library(dplyr)
library(stringr)

homicide_data <- get_cansim("35-10-0156-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

homicide_rates <- 
  homicide_data %>%
  filter(
    UOM == "Rate per 100,000 population",
    !is.na(VALUE)
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Gender,
    `Indigenous identity`,
    Value = VALUE
  ) %>%
  left_join(geocodes) %>%
  mutate(
    `Indigenous identity` = ifelse(str_detect(`Indigenous identity`, "Total"), "Total", `Indigenous identity`),
     GeoCode = ifelse(Gender == "All genders" & `Indigenous identity` == "Total", GeoCode, NA)
  ) %>%
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    homicide_rates %>%
      filter(Geography == "Canada", Gender == "All genders", `Indigenous identity` == "Total") %>%
      mutate(across(Geography:`Indigenous identity`, ~ "")),
    homicide_rates %>%
      filter(!(Geography == "Canada" & Gender == "All genders" & `Indigenous identity` == "Total"))
  )

write.csv(
  data_final, 
  "data/indicator_16-1-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
