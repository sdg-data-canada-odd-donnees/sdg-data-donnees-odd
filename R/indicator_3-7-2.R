# Indicator 3.7.2 ---------------------------------------------------------
# Adolescent birth rate (aged 10–14 years; aged 15–19 years) per 1,000 women in that age group

library(cansim)
library(dplyr)
library(stringr)

birth_rates <- get_cansim("13-10-0418-01", factors = FALSE)

adolescent_birth_rates <-
  birth_rates %>%
  filter(
    REF_DATE >= 2015,
    Characteristics == "Age-specific fertility rate, females 15 to 19 years"
  ) %>%
  mutate(
    GEO = str_remove(GEO, ", place of residence of mother")
  ) %>%
  select(
    Year = REF_DATE,
    `Place of residence of mother` = GEO,
    Value = VALUE
  )

total_line <- 
  adolescent_birth_rates %>%
  filter(
    `Place of residence of mother` == "Canada"
  ) %>%
  mutate_at(c("Place of residence of mother"), ~ "")  

data_final <- bind_rows(total_line, adolescent_birth_rates)  

write.csv(
  data_final,
  "data/indicator_3-7-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
