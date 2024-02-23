# 4.1.2 ------------------------------------------------------------------

library(dplyr)
library(cansim)
library(stringr)

completion_rate <- get_cansim("37-10-0221-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

exclude_Canada <- c(
  "Canada"
)

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
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

total_line <- 
  rate %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(Geography = "")  

data_with_Canada <- bind_rows(total_line, rate)

data_final <-
  data_with_Canada %>%
  filter(
    !Geography %in% exclude_Canada
  ) 

write.csv(
  data_final,
  "data/indicator_4-1-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
