# Indicator 3.2.2 ---------------------------------------------------------
# Neonatal mortality rate

library(cansim)
library(dplyr)
library(stringr)

neonat_mortality_data <- get_cansim("13-10-0712-01", factors = FALSE)

neonat_mort_rate <- 
  neonat_mortality_data %>%
  filter(
    REF_DATE >= 2015,
    `Age at time of death` == "Neonatal, age at time of death, 0 to 27 days"
  ) %>%
  mutate(
    `Age at time of death` = str_remove(`Age at time of death`, ", age at time of death,")
  ) %>%
  select(
    Year = REF_DATE,
    Sex,
    Units = Characteristics,
    Value = VALUE
  )

total_line <-
  neonat_mort_rate %>%
  filter(
    Sex == "Both sexes", 
  ) %>%
  mutate_at(c("Sex"), ~ "")

data_final <-
  bind_rows(
    total_line,
    neonat_mort_rate %>%
      filter(!(Sex == "Both sexes"))
  )

write.csv(
  data_final,
  "data/indicator_3-2-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
