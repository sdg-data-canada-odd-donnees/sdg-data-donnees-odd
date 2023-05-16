library(dplyr)
library(cansim)

household_energy_consumption <- get_cansim("25-10-0060-01", factors = FALSE)
dwelling_energy_consumption <- get_cansim("25-10-0061-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

household_energy_consumption <- 
  household_energy_consumption %>% 
  filter(
    REF_DATE >= 2015,
    `Energy consumption` == "Gigajoules per household"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Energy type`,
    Value = VALUE
  ) %>% 
  mutate(
    `Type of dwelling` = "Total"
  ) %>% 
  relocate(`Type of dwelling`, .after = Geography)

dwelling_energy_consumption <- 
  dwelling_energy_consumption %>% 
  filter(
    REF_DATE >= 2015,
    `Energy consumption` == "Gigajoules per household"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of dwelling`,
    `Energy type`,
    Value = VALUE
  ) 

total_energy_consumption <- 
  bind_rows(
    dwelling_energy_consumption,
    household_energy_consumption
)

total_line <- 
  household_energy_consumption %>% 
  filter(
    Geography == "Canada",
    `Type of dwelling` == "Total",
    `Energy type` == "Total, all energy types"
  ) %>% 
  mutate_at(2:4, ~ "")

data_final <- 
  bind_rows(
    total_line,
    total_energy_consumption %>% 
    filter(
      !(Geography == "Canada" &
      `Type of dwelling` == "Total" &
      `Energy type` == "Total, all energy types")
    )
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

write.csv(data_final, "data/indicator_7-1-1.csv", row.names = FALSE, na = "")
