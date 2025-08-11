# Indicator 5.5.2 ---------------------------------------------------------

# 14-10-0416-01

library(cansim)
library(dplyr)
library(stringr)

# load geocode
geocodes <- read.csv("geocodes.csv")

repr_in_mgmt <- get_cansim("14-10-0416-01", factors = FALSE)

occupations <- c(
  "Management occupations [00, 10, 20, 30, 40, 50, 60, 70, 80, 90]",
  "Legislative and senior management occupations [0]",
  "Specialized middle management occupations [10, 20, 30, 40, 50]",
  "Middle management occupations in retail and wholesale trade and customer services [60]",
  "Middle management occupations in trades, transportation, production and utilities [70, 80, 90]"
)

clean_repr_in_mgmt <-
  repr_in_mgmt %>%
  filter(
    REF_DATE >= 2015,
    `Labour force characteristics` == "Proportion of employment",
    Gender == "Women+",
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
  mutate_at(c("Geography", "Occupation"), ~NA)

data_final <-
  bind_rows(
    total_line,
    clean_repr_in_mgmt %>%
      filter(!(Geography == "Canada" & Occupation == "Management occupations"))
  ) %>%
  mutate(
    Progress = 50 + abs(Value - 50)
  ) %>%
  select(
    Year,
    Occupation,
    Geography,
    Progress,
    Value
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_5-5-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
