
# 12.4.2 ------------------------------------------------------------------

library(dplyr)
library(cansim)

waste <- get_cansim("38-10-0155-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

selected_waste_type <- c(
  "Leftover or expired medication",
  "Medical sharps",
  "Leftover paint or solvents",
  "Unwanted engine oil or anti-freeze",
  "Dead or unwanted batteries (excluding car batteries)",
  "Leftover pesticides",
  "Mattresses",
  "Textiles"
)

data_final <- 
  waste %>% 
  filter(
    `Household hazardous waste` %in% selected_waste_type
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Household hazardous waste`,
    Value = VALUE
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

write.csv(
   data_final,
   "data/indicator_12-4-2.csv",
   na = "",
   row.names = FALSE
)
