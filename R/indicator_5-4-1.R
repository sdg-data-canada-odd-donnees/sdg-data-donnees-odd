# Indicator 5.4.1 ---------------------------------------------------------
# Proportion of time spent on unpaid domestic and care work, by sex, age and location

library(cansim)
library(dplyr)
library(stringr)

# load CODR table from stc api
raw_data_2015 <- get_cansim("45-10-0014-01", factors = FALSE) #archived and inactive
raw_data_2022 <- get_cansim("45-10-0104-01", factors = FALSE)

filtered_2015 <- raw_data_2015 %>%
  filter(
    str_starts(`Hierarchy for Activity group`, "11"),
    Statistics == "Proportion of day, population"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Activity group`,
    `Age group`,
    Gender = Sex,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  mutate(
    # Rename gender variables to align with most recent data
    Gender = case_match(
      Gender,
      "Both sexes" ~ "Total, all persons",
      "Male" ~ "Men+",
      "Female" ~ "Women+"
    ),
    # Rename activity group categories to align with most recent data
    `Activity group` = case_match(
      `Activity group`,
      "Household chores" ~ "Unpaid household work",
      "Care of household children under 18 years" ~ "Care of children under 18 years",
      "Care of household adults" ~ "Care of adults",
      # "Shopping for goods or services" ~ "Shopping for goods or services",
      .default = `Activity group`
    )
  )

filtered_2022 <- raw_data_2022 %>%
  filter(
    str_starts(`Hierarchy for Activity group`, "11"),
    Statistics == "Proportion of day, population"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Activity group`,
    `Age group`,
    Gender,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  mutate(
    # Simplify activity group category names
    `Activity group` = str_remove(`Activity group`, " \\(household or family\\)")
  )

unpaid_work_time <- bind_rows(filtered_2015, filtered_2022) %>%
  mutate(
    # Rename unpaid work activities category for clarity
    `Activity group` = replace(`Activity group`, `Activity group` == "Unpaid work activities", "Total, unpaid work activities"),
    # Remove geocode for Canada
    GeoCode = replace(GeoCode, GeoCode == 11124, NA),
    # Set headline
    across(
      c("Geography", "Activity group", "Age group", "Gender", "GeoCode"),
      ~ replace(., 
                Geography == "Canada" & 
                `Activity group` == "Total, unpaid work activities" &
                `Age group` == "Total, 15 years and over" &
                Gender == "Total, all persons",
                NA)
    )
  )

write.csv(unpaid_work_time, "data/indicator_5-4-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
