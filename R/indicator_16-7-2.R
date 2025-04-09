# Indicator 16.7.2 ---------------------------------------------------------
# Proportion of population who believe decision-making is inclusive and responsive, by sex, age, disability and population group

# load libraries
library(cansim)
library(dplyr)

data_raw <- get_cansim("43-10-0062-01", factors = FALSE)

selected_indicators <- c(
  "Confidence in the justice system and courts",
  "Confidence in Federal Parliament"
)

data_final <-  data_raw %>%
  filter(
    Statistics == "Percentage of persons",
    Indicators %in% selected_indicators
  ) %>%
  mutate(
    `Visible minority` = case_match(
      `Visible minority`,
      "Total, by visible minority group" ~ "Total population",
      "Total – Visible minority" ~ "Total visible minority population",
      .default = `Visible minority`
    ),
    COMMENT_OBS = replace(STATUS, STATUS == "E", "Use with caution")
  ) %>%
  select(
    Year = REF_DATE,
    Institutions = Indicators,
    `Selected sociodemographic characteristics`,
    `Visible minority`,
    COMMENT_OBS,
    Value = VALUE
  )

write.csv(data_final, "data/indicator_16-7-2.csv",
  na = "", row.names = FALSE, fileEncoding = "UTF-8")