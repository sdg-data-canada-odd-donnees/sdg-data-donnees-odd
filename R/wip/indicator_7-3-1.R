
# Indicator 7.3.1 ---------------------------------------------------------
library(cansim)
library(dplyr)

# there's an issue with the stc api and this table
# gdp <- get_cansim("36-10-0434-03")
gdp <- read_csv("raw data/annual_gdp.csv", col_types = list(Year = "c"))
energy_use <- get_cansim("38-10-0096-01")

final_data <- 
  left_join(
    energy_use %>% 
      filter(Sector == "Total, industries and households", REF_DATE >= 2015) %>% 
      select(Year = REF_DATE, Geography = GEO, energy = VALUE),
    gdp,
    by = "Year"
  ) %>% 
  mutate(
    Value = energy/Value
  ) %>% 
  select(Year, Value)

write_csv(final_data, "data/indicator_7-3-1.csv")


