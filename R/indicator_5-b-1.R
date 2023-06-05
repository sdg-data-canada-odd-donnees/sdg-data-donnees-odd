# Indicator 5.b.1 ---------------------------------------------------------
# 5.b.1 Proportion of individuals who own a mobile telephone, by sex

library(dplyr)
library(cansim)

# get CODR table
dwelling_equipment <- cansim::get_cansim("11-10-0228-01", factors = FALSE)

# get geocodes to join with data
geocodes <- read.csv("geocodes.csv")

household_cellphones <- 
  dwelling_equipment %>%
  filter(
    REF_DATE >= 2015,
    UOM == "Percent",
    `Dwelling characteristics and household equipment` == "Households having cellular telephones"
    ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .after = Geography) %>%
  mutate(Geography = ifelse(Geography == "Canada", "", Geography))

write.csv(
  household_cellphones, 
  "data/indicator_5-b-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
