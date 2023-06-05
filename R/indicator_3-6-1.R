# Indicator 3.6.1 ---------------------------------------------------------
#  Death rate due to road traffic injuries

library(cansim)
library(dplyr)
library(stringr)

deaths <- get_cansim("13-10-0392-01", factors = FALSE)

transport_deaths <-
  deaths %>%
    mutate(
      `Cause of death (ICD-10)` = str_trim(str_remove_all(`Cause of death (ICD-10)`, "\\[.*\\]")),
      `Age at time of death` = str_remove(`Age at time of death`, "Age at time of death, "),
      `Age at time of death` = str_to_sentence(`Age at time of death`)
    ) %>%
    filter(
      REF_DATE >= 2015,
      `Cause of death (ICD-10)` %in% c("Transport accidents", "Motor vehicle accidents", "Other land transport accidents"),
      Characteristics == "Age-specific mortality rate per 100,000 population"
    ) %>%
    select(
      Year = REF_DATE,
      Age = `Age at time of death`,
      Sex,
      `Cause of death` = `Cause of death (ICD-10)`,
      Value = VALUE
    )

total_line <-
  transport_deaths %>%
  filter(
    Age == "All ages",
    Sex == "Both sexes",
    `Cause of death` == "Transport accidents"
  ) %>%
  mutate_at(c("Sex", "Age", "Cause of death"), ~ "")  

data_final <- 
  bind_rows(
    total_line, 
    transport_deaths %>%
      filter(!(Age == "All ages" & Sex == "Both sexes" & `Cause of death` == "Transport accidents"))
    )  

write.csv(
  data_final,
  "data/indicator_3-6-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)