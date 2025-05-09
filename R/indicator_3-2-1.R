# Indicator 3.2.1 ---------------------------------------------------------
# Under‐5 mortality rate

library(cansim)
library(dplyr)
library(stringr)

# get CODR table(s)
deaths_by_age <- get_cansim("13-10-0709-01", factors = FALSE)
live_births <- get_cansim("13-10-0414-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

# dimensions to exclude from live birth totals
excluded_place_of_birth <- c(
  "United States, place of occurrence",
  "Total, Canada and United States, place of occurrence",
  "Northwest Territories including Nunavut",
  "Unknown province or territory of residence"
)

# get deaths for under 5
deaths_under_5 <- 
  deaths_by_age %>%
  filter(
    REF_DATE >= 2015,
    Sex == "Both sexes",
    `Age at time of death` == "Age at time of death, 0 to 4 years",
    !GEO %in% excluded_place_of_birth
  ) %>% 
  mutate(
    GEO = str_remove(GEO, ", place of residence")
  ) %>%
  select(Year = REF_DATE, Geography = GEO, deaths_u5 = VALUE) %>%
  na.omit()

# Total live births (place of occurrence in Canada)
total_live_births <- 
  live_births %>% 
  filter(
    REF_DATE >= 2015,
    GEO == "Total, Canada and place of residence of mother outside Canada",
    !`Geography, place of occurrence` %in% excluded_place_of_birth
  ) %>% 
  mutate(
    `Geography, place of occurrence` = str_remove(`Geography, place of occurrence`, ", place of occurrence")
  ) %>%
  select(
    Year = REF_DATE, 
    Geography = `Geography, place of occurrence`, 
    total_live_births = VALUE
  ) %>%
  na.omit()

# Calculate mort rate for under 5 yo
u5_mort_rate <- 
  deaths_under_5 %>%
  left_join(total_live_births) %>%
  # Calculate mort rate
  transmute(
    Year, 
    Geography, 
    Value = round((deaths_u5/total_live_births)*1000, 2)
  ) %>%
  filter(Geography != "Unknown province or territory of residence") %>%
  # Create total line
  mutate(Geography = ifelse(Geography == "Canada", "", Geography)) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

write.csv(
  u5_mort_rate,
  "data/indicator_3-2-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

