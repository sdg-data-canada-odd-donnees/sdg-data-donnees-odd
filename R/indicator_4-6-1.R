## GIF 4.6.1

# load libraries
library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

proficiency <- get_cansim("37-10-0047-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

data_final <-
  proficiency %>%
  filter(
    !Statistics == "Average score"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    `Age group`,
    Skill,
    `Proficiency level` = Statistics,
    Value = VALUE
  ) %>%
  mutate(
    `Age group` = str_remove_all(`Age group`, " age groups"),
    `Proficiency level` = str_remove_all(`Proficiency level`, "Percent of the population scoring at proficiency level ")
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

# Write the csv file
write.csv(data_final, 
          "data/indicator_4-6-1.csv",
          na = "", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")