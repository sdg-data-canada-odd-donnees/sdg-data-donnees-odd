# Indicator 5.5.2 ---------------------------------------------------------

# 14-10-0335-01

library(cansim)
library(dplyr)
library(stringr)

repr_in_mgmt <- get_cansim("14-10-0335-01", factors = FALSE)

occupations <- c(
  "Management occupations [0]",                                                 
  "Senior management occupations [00]",
  "Specialized middle management occupations [01-05]",
  "Middle management occupations in retail and wholesale trade and customer services [06]",
  "Middle management occupations in trades, transportation, production and utilities [07-09]"
)

clean_repr_in_mgmt <-
  repr_in_mgmt %>% 
  filter(
    REF_DATE >= 2015,
    `Labour force characteristics` == "Proportion of employment",
    Sex == "Females",
    `National Occupational Classification (NOC)` %in% occupations
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Occupation = `National Occupational Classification (NOC)`,
    Value = VALUE
  ) %>%
  mutate(
    Occupation = str_trim(str_remove(Occupation, "\\[.*\\]"))
  )

total_line <- 
  clean_repr_in_mgmt %>%
  filter(
    Geography == "Canada",
    Occupation == "Management occupations"
  ) %>%
  mutate_at(c("Geography", "Occupation"), ~ "")

data_final <- 
  bind_rows(
    total_line,
    clean_repr_in_mgmt %>%
      filter(!(Geography == "Canada" & Occupation == "Management occupations"))
  )

write.csv(
  data_final,
  "data/indicator_5-5-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
