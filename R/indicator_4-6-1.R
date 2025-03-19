## GIF 4.6.1

# load libraries
library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

proficiency_archived_raw <- get_cansim("37-10-0047-01", factors = FALSE)
proficiency_raw <- get_cansim("37-10-0259-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

proficiency_archived <-
  proficiency_archived_raw %>%
  filter(
    !Statistics == "Average score"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Gender = Sex,
    `Age group`,
    Skill,
    `Proficiency level` = Statistics,
    Value = VALUE
  ) %>%
  mutate(
    `Age group` = str_remove_all(`Age group`, ", age groups"),
    `Proficiency level` = str_remove_all(`Proficiency level`, "Percent of the population scoring at proficiency level "),
    Gender = case_when(
      Gender == "Both sexes" ~ "Total",
      Gender == "Males" ~ "Men+",
      Gender == "Females" ~ "Women+"
    )
  ) %>%
  na.omit()

proficiency <-
  proficiency_raw %>%
  filter(
    !Statistics %in% c("Average score", "Standard error, average score", "Standard error, level 1 or below", "Standard error, level 2", "Standard error, level 3 or above"),
    !GEO == "Organisation for Economic Co-operation and Development (OECD)",
    !Skill == "Adaptive problem solving"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Gender,
    `Age group` = `Age Group`,
    Skill,
    `Proficiency level` = Statistics,
    Value = VALUE
  ) %>%
  mutate(
    `Age group` = str_remove_all(`Age group`, ", age groups"),
    `Gender` = str_remove_all(`Age group`, ", gender"),
    `Proficiency level` = str_remove_all(`Proficiency level`, "Percentage of the population scoring at proficiency level ")
  ) %>%
  na.omit()

proficiency_sum <-
  proficiency %>%
  filter(`Proficiency level` %in% c("2", "1 or below")) %>%
  group_by(Year, Geography, Gender, `Age group`, Skill) %>%
  summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(`Proficiency level` = "2 or below")

proficiency_combined <-
  bind_rows(proficiency_sum,proficiency) %>%
  filter(!`Proficiency level` %in% c("2", "1 or below"))

data_final <-
  bind_rows(proficiency_archived,proficiency_combined) %>%
  select(
    Year,
    `Proficiency level`,
    Skill,
    Geography,
    Gender,
    `Age group`,
    Value
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

# Write the csv file
write.csv(data_final, 
          "data/indicator_4-6-1.csv",
          na = "", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")