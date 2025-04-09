# Indicator 9.2.2 ------------------------------------------------------
# Manufacturing employment as a proportion of total employment

library(cansim)
library(dplyr)
library(stringr)

employment_data <- get_cansim("14-10-0202-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

manufacturing_employment <-
  employment_data %>%
  filter(
    REF_DATE >= 2015,
    str_starts(
      `Hierarchy for North American Industry Classification System (NAICS)`, 
      "1.2.3.34"
      ) | `Hierarchy for North American Industry Classification System (NAICS)` == "1",
    `Type of employee` == "All employees"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industry = `North American Industry Classification System (NAICS)`,
    Value = VALUE
  ) %>%
  mutate(
    Industry = str_remove(Industry, " \\[.*\\]")
  ) %>%
  na.omit()

manufacturing_employment <- 
  manufacturing_employment %>% 
  left_join(
    manufacturing_employment %>% 
      filter(Industry == "Industrial aggregate including unclassified businesses") %>% 
      rename(Total = Value) %>% 
      select(-Industry) 
  ) %>% 
  filter(Industry != "Industrial aggregate including unclassified businesses") %>%
  transmute(
    Year, Geography, 
    Industry = ifelse(Industry == "Manufacturing", "Total manufacturing", Industry), 
    Value = round((Value/Total)*100, 2)
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    manufacturing_employment %>%
      filter(Geography == "Canada", Industry == "Total manufacturing") %>%
      mutate(across(2:(ncol(.)-2), ~ "")),
    manufacturing_employment %>%
      filter(!(Geography == "Canada" & Industry == "Total manufacturing"))
  ) %>%
  select(
    Year,
    Industry,
    Geography,
    GeoCode,
    Value
  )

write.csv(
  data_final, 
  "data/indicator_9-2-2.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
