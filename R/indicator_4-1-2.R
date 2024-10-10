# GIF 4.1.2 ------------------------------------------------------------------

library(dplyr)
library(cansim)
library(stringr)

completion_rate <- get_cansim("37-10-0221-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

rate <- 
  completion_rate %>%
  mutate(
    Gender = str_remove(Gender, " gender")
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Graduation rate`,
    Gender,
    Value = VALUE
  )

total_line <- 
  rate %>%
  filter(
    Geography == "Canada",
    `Graduation rate` == "Extended-time",
    Gender == "Total"
  ) %>%
  mutate_at(2:4, ~ "")

data_final <-
  bind_rows(total_line, rate) %>%
  filter(
    !(Geography == "Canada" &
      `Graduation rate` == "Extended-time" &
      Gender == "Total")
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_4-1-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
