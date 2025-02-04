# Indicator 16.b.1 ---------------------------------------------------------
# Proportion of population reporting having personally felt discriminated against or harassed in the previous 12 months on the basis of a ground of discrimination prohibited under international human rights law

# load libraries
library(cansim)
library(dplyr)
library(stringr)

discrimination_raw <- get_cansim("45-10-0100-01", factors = FALSE)

# Load geocodes from a CSV file
geocodes <- read.csv("geocodes.csv")

discrimination <- 
  discrimination_raw %>%
  filter(
    Statistics == "Percentage of persons",
    !GEO %in% c("Atlantic Region", "Prairies Region"),
  ) %>%
  mutate(
    REF_DATE = str_replace_all(REF_DATE, "-01", " Q1"),
    REF_DATE = str_replace_all(REF_DATE, "-04", " Q2"),
    REF_DATE = str_replace_all(REF_DATE, "-07", " Q3"),
    REF_DATE = str_replace_all(REF_DATE, "-10", " Q4"),
    Gender = str_replace_all(Gender, "Total, all persons", "Total")
  ) %>%
  select(
    Year = REF_DATE,
    `Discrimination Indicators` = Indicators,
    Geography = GEO,
    Gender,
    Value = VALUE
  ) %>%
  mutate(
    Geography = str_remove(Geography, " \\(.*\\)")
  ) %>%
  na.omit()
  
total_line <-
  discrimination %>%
  filter(
    `Discrimination Indicators` == "Experienced discrimination or unfair treatment in Canada",
    Geography == "Canada",
    Gender == "Total"
  ) %>% 
  mutate_at(2:(ncol(.) - 1), ~ "")

non_total <-
  discrimination %>%
  filter(
    !(
      `Discrimination Indicators` == "Experienced discrimination or unfair treatment in Canada"&
      Geography == "Canada"&
      Gender == "Total"
    )
  )

data_final <-
  bind_rows(total_line,non_total) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

write.csv(
  data_final,
  "data/indicator_16-b-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)