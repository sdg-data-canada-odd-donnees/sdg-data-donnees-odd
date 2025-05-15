# Indicator 16.2.3 ---------------------------------------------------------
# Proportion of young women and men aged 18–29 years who experienced sexual violence by age 18

library(cansim)
library(dplyr)
library(stringr)

sexual_violence_data <- get_cansim("35-10-0167-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

age <- c(
  "15 to 24 years",
  "25 to 34 years"
)

data_final <-
  sexual_violence_data %>%
  filter(
    `Selected demographic characteristics` %in% age,
    UOM == "Percent",
    Statistics == "Percentage",
    `Type of childhood maltreatment` == "Sexual abuse",
    !is.na(VALUE)
  )  %>%
  mutate(
    `Age group` = `Selected demographic characteristics`
  ) %>%
  select(
    Year = REF_DATE,
    Gender,
    `Age group`,
    Geography = GEO,
    Value = VALUE
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = Value)

write.csv(
  data_final, 
  "data/indicator_16-2-3.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
