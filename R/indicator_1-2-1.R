# Indicator 1.2.1: ------------------------------------------------
# Proportion of population living below the national poverty line, by sex and age

# Load packages
library(cansim)
library(dplyr)

# get CODR table
raw_table <- get_cansim("11-10-0135-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

demographics <- c(
  "All persons",
  "Females",
  "Females, 18 to 64 years",
  "Females, 65 years and over",
  "Females, under 18 years",
  "Males",
  "Males, 18 to 64 years",
  "Males, 65 years and over",
  "Males, under 18 years",
  "Persons 18 to 64 years",
  "Persons 65 years and over",
  "Persons under 18 years"
)

# filter for years past 2015, the proportion (rather than number), and remove "economic family" grouping
low_income <- 
  raw_table %>%
  rename(hier = `Hierarchy for Persons in low income`) %>%
  mutate(REF_DATE = as.numeric(REF_DATE)) %>%
  filter(
    REF_DATE >= 2015, 
    Statistics=="Percentage of persons in low income",
    `Persons in low income` %in% demographics,
    `Low income lines` == "Market basket measure, 2018 base"
    ) %>%
  select(
    Year = REF_DATE, 
    Geography = GEO, 
    `Persons in low income`, 
    Value = VALUE
  )

# create total line for Open SDG format (making totals blank)
total_line <-
  low_income %>%
  filter(
    Geography=="Canada", 
    `Persons in low income` == "All persons"
  ) %>%
  mutate(
    Geography = "", 
    `Persons in low income`= ""
  )

# combine total lines with all disaggregations
low_income <- 
  bind_rows(
    total_line,
    low_income %>% 
      filter(!(Geography=="Canada" & `Persons in low income`=="All persons")) %>%
      arrange(Geography, `Persons in low income`, Year)
  )

# write data
write.csv(
  low_income,
  "data/indicator_1-2-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
