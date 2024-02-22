# GIF 3.1.2 ---------------------------------------------------------

library(cansim)
library(dplyr)
library(stringr)

birth_data <- get_cansim("13-10-0429-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

excluded_dimensions <- c(
  "Unknown province or territory, place of residence of mother"
)

hospital <- 
  birth_data %>%
  filter(
    REF_DATE >= 2015,
    `Place of birth` == "Place of birth, hospital",
    Characteristics == "Percentage",
    !GEO %in% excluded_dimensions
  ) %>% 
  mutate(
    GEO = str_remove(GEO, ", place of residence of mother")
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

total_line <- 
  hospital %>%
  filter(
    Geography == "Canada",
  ) %>%
  mutate(Geography = "")  

data_final <- bind_rows(total_line, hospital) 

write.csv(
  data_final,
  "data/indicator_3-1-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


