# 11.1.1 ------------------------------------------------------------------

# Large urban population centres have a population of 100,000 or more.

library(cansim)
library(dplyr)
library(stringr)

core_housing <- get_cansim("46-10-0065-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

core_housing <- 
  core_housing %>%
  filter(
    `Core housing need statistics` == "Percentage of persons in core housing need",
    str_detect(GEO, "urban")
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Tenure = `Tenure including first-time homebuyer and social and affordable housing status`,
    Value = VALUE
  ) %>% 
  mutate(
    Geography = str_remove(Geography, "Large urban population centres, ")
  ) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = "Value")

total_line <- 
  core_housing %>% 
  filter(
    Geography == "Total, large urban population centres",
    Tenure == "Total, tenure"
  ) %>% 
  mutate(across(2:3, ~ ""))

data_final <-
  bind_rows(
    total_line,
    core_housing %>%
      filter(!(
        Geography == "Total, large urban population centres" &
          Tenure == "Total, tenure"
      ))
  )

write.csv(
  data_final,
  "data/indicator_11-1-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
