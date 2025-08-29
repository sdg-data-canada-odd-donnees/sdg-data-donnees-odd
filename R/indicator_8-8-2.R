# GIF 8.8.2 ---------------------------------------------------------------

# Unit of measure: Score
# Scores for this indicator range from 0 to 10, with 0 being the best possible score (indicating higher levels of compliance with FACB rights) and 10 being the worst score (indicating lower levels of compliance with FACB rights).

# load libraries
library(dplyr)
library(stringr)
library(tidyr)

ilostat <- read.csv("https://rplumber.ilo.org/data/indicator/?id=SDG_0882_NOC_RT_A&ref_area=CAN&timefrom=2015&timeto=2023&type=code&format=.csv")

data_final <-
  ilostat %>%
  select(
    Year = time,
    Value = obs_value
  ) %>%
  arrange(Year)

write.csv(
  data_final, 
  "data/indicator_8-8-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)