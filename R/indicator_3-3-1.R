# GIF 3.3.1

options(timeout = 300)
library(dplyr)
library(purrr)
library(rjson)

# Function to extract and flatten JSON into a table
extract_json_to_table <- function(url) {
  raw <- rjson::fromJSON(file = url)
  
  series_name <- raw$series
  
  # Flatten "dimensions" into a data frame
  df <- map_dfr(raw$dimensions, function(x) {
    tibble(
      Year = x$timePeriodStart,
      Series = series_name,
      `Age` = x$`Age`,
      `Sex` = x$`Sex`,
      Value = x$value
    )
  })
  
  return(df)
}


hiv_url <- "https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Series/SH_HIV_INCD/GeoArea/124/DataSlice"

hiv_data <- extract_json_to_table(hiv_url)

data_final <-
  hiv_data %>%
  filter(
    !(Value == "NaN")
  ) %>%
  mutate(
    Age = case_when(
      Age == "ALLAGE" ~ "All ages",
      Age == "15-24" ~ "15 to 24 years",
      Age == "15-49" ~ "15 to 49 years",
      Age == "50+" ~ "50 years and over"
    ),
    Sex = case_when(
      Sex == "BOTHSEX" ~ "Both sexes",
      Sex == "FEMALE" ~ "Female",
      Sex == "MALE" ~ "Male"
    )
  ) %>%
  mutate(
    Age = case_when(
      Age == "All ages" & Sex == "Both sexes" ~ "",
      TRUE ~ Age
    ),
    Sex = case_when(
      Age == "" & Sex == "Both sexes" ~ "",
      TRUE ~ Sex
    ),
    Year = as.numeric(Year),   # convert to numeric
    Value = as.numeric(Value)  # ensure Value is numeric too
  ) %>%
  arrange(Age, Sex) %>%   # order ascending by year
  rename(`Age group` = Age) %>%
  select(-`Series`)

write.csv(
  data_final, 
  "data/indicator_3-3-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)