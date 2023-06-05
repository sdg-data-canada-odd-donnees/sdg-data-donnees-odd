# GIF 3.1.1 ---------------------------------------------------------

library(cansim)
library(dplyr)

deaths <- get_cansim("13-10-0152-01", factors = FALSE)
live_births <- get_cansim("13-10-0414-01", factors = FALSE)

# Total deaths due to pregnancy et al
total_deaths <- 
  deaths %>%
  filter(
    REF_DATE >= 2015,
    `Cause of death (ICD-10)` == "Chapter XV: Pregnancy, childbirth and the puerperium [O00-O99]",
    `Age group` == "Total, all ages",
    `Sex` == "Both sexes"
    ) %>% 
  select(REF_DATE, total_deaths = VALUE)

# Total live births (residence in Canada)
total_live_births <- 
  live_births %>%
  filter(
    REF_DATE >= 2015,
    `Geography, place of occurrence` == "Canada, place of occurrence",
    GEO == "Canada, place of residence of mother"
    ) %>%
  select(REF_DATE, total_live_births = VALUE)

# Calculate MMR
data_final <-
  total_deaths %>%
  left_join(total_live_births) %>%
  transmute(
    REF_DATE, 
    Value = round((total_deaths/total_live_births)*100000, 2)
  ) %>%
  rename(Year = REF_DATE)

write.csv(
  data_final,
  "data/indicator_3-1-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)