# Indicator 8.6.1 ------------------------------------------------------
# Proportion of youth (aged 15-24 years) not in education, employment or
# training

library(dplyr)
library(cansim)
library(stringr)

NEET <- get_cansim("37-10-0196-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

exclude_Canada <- c(
  "Canada"
)

exclude_age <- c(
  "25 to 29 years",
  "Total, 15 to 29 years"
)

selected_status <- c(
  "Sub-total, not in employment, education or training (NEET)",
  "Unemployed, Not in employment, education or training (NEET)",
  "Not in the labour force, Not in employment, education or training (NEET)"
)

NEET_filtered <- 
  NEET %>% 
  filter(
    REF_DATE >= 2015,
    GEO != "Organisation for Economic Co-operation and Development (OECD) - average",
    `Labour force and education status` %in% selected_status,
    !`Age group` %in% exclude_age,
    Statistics == "Proportion"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Status = `Labour force and education status`,
    Gender,
    `Age group`,
    Value = VALUE
  ) %>% 
  mutate(
    Status = str_replace(Status, "Sub-total", "Total")
  ) %>% 
  filter(!is.na(Value)) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

data_final <- NEET_filtered %>%
  mutate(
    across(
      c("Geography", "Status", "Gender", "Age group"),
      ~ replace(., `Geography` == "Canada" & Status == "Total, not in employment, education or training (NEET)" & Gender == "Both genders" & `Age group` == "15 to 24 years", NA)
    )
  ) %>%
  select(
    Year,
    Status,
    Geography,
    Gender,
    `Age group`,
    GeoCode,
    Value
  )

# total_line <-
#   NEET_filtered %>%
#   filter(Geography == "Canada",
#          Status == "Total, not in employment, education or training (NEET)",
#          Gender == "Both genders",
#          `Age group` == "15 to 24 years") %>%
#   mutate(Geography = "", Status = "", Gender = "", `Age group` = "") %>%
#   filter(!is.na(Value))
# 
# data_with_Canada <- bind_rows(total_line, NEET_filtered)

# data_final <-
#   data_with_Canada %>%
#   filter(
#     !Geography %in% exclude_Canada
#   ) 

write.csv(
  data_final,
  "data/indicator_8-6-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

