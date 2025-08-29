# GIF 4.a.1

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
      `Education level` = x$`Education level`,
      Value = x$value
    )
  })
  
  return(df)
}

computer_url <- "https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Series/SE_ACS_CMPTR/GeoArea/124/DataSlice"
internet_url <- "https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Series/SE_ACS_INTNT/GeoArea/124/DataSlice"

computer_df <- extract_json_to_table(computer_url)
internet_df <- extract_json_to_table(internet_url)

data_final <- bind_rows(computer_df, internet_df) %>%
  mutate(
    Series = case_when(
      Series == "SE_ACS_CMPTR" ~ "Proportion of schools with access to computers for pedagogical purposes",
      Series == "SE_ACS_INTNT" ~ "Proportion of schools with access to the Internet for pedagogical purposes"
    ),
    `Education level` = case_when(
      `Education level` == "PRIMAR" ~ "Primary",
      `Education level` == "SECOND" ~ "Secondary"
    )
  ) %>%
  mutate(
    `Education level` = case_when(
      Series == "Proportion of schools with access to the Internet for pedagogical purposes" & 
        `Education level` == "Secondary" ~ "",
      TRUE ~ `Education level`
    ),
    Year = as.numeric(Year),   # convert to numeric
    Value = as.numeric(Value)  # ensure Value is numeric too
  ) %>%
  arrange(Series, Year)   # order ascending by year

write.csv(
  data_final, 
  "data/indicator_4-a-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)