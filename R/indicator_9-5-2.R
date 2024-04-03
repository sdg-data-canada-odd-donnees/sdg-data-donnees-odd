# Indicator 9.5.2 ------------------------------------------------------
# 9.5.2 Researchers (in full-time equivalent) per million inhabitants

library(cansim)
library(dplyr)

rd_personnel <- get_cansim("27-10-0022-01", factors = FALSE)
pop_ests <- get_cansim("17-10-0005-01", factors = FALSE)

rd_personnel %>% 
  distinct(GEO)

researchers <- 
  rd_personnel %>%
  filter(
    REF_DATE >= 2010,
    `Occupational category` == "Researchers",
    `Performing sector` == "Total, performing sectors",
    `Type of science` == "Total sciences"
  ) %>%
  select(
    Year = REF_DATE,
    #`Performing sector`,
    `Occupational category`,
    No_Personnel = VALUE
  ) %>%
  group_by(Year) %>%
  summarise(No_Personnel = sum(No_Personnel), .groups = "drop")

population <- 
  pop_ests %>%
  filter(
    REF_DATE >= 2010,
    GEO == "Canada",
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>%
  select(
    Year = REF_DATE,
    `Age group`,
    VALUE
  ) %>% 
  group_by(Year) %>% 
  summarise(Population = sum(VALUE), .groups = "drop")

data_final <- 
  left_join(researchers, population) %>% 
  transmute(
    Year, 
    Value = round((No_Personnel/Population)*1000000, 2)
  )

write.csv(
  data_final, 
  "data/indicator_9-5-2.csv", 
  na = "", 
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

