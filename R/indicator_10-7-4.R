# Indicator 10.7.4 ---------------------------------------------------------
# Proportion of the population who are refugees, by country of origin

options(scipen = 999)
options(timeout = 1000) 
library(cansim)
library(dplyr)
library(stringr)

stop("Temporary skip 10.7.4") # The Census CODR table is taking a long time to parse, so this script is temporarily skipped to not time out on GitHub Actions.

un_pop <- read.csv("un_pop.csv")

un_pop_filtered <- un_pop %>%
  filter(
    Year %in% c(
      "1990",
      "2000",
      "2010",
      "2015",
      "2021"),
    !Place.of.birth == "Canada"
  ) %>%
  select(
    Year,
    `Place of birth` = Place.of.birth,
    UN_Pop
  )

canada <- c(
  "Canada",
  "Newfoundland and Labrador",
  "Prince Edward Island",
  "Nova Scotia",
  "New Brunswick",
  "Quebec",
  "Ontario",
  "Manitoba",
  "Saskatchewan",
  "Alberta",
  "British Columbia",
  "Yukon",
  "Northwest Territories",
  "Nunavut"
)

canada_pop <- get_cansim("17-10-0005-01") 

canada_pop_filtered <- canada_pop %>%
  filter(
    Gender == "Total - gender",
    `Age group` == "All ages",
    REF_DATE %in% c(
      "1990",
      "2000",
      "2010",
      "2015",
      "2021"),
    GEO == "Canada"
  ) %>%
  select(
    Year = REF_DATE,
    `Place of birth` = GEO,
    Can_Pop = VALUE
  )

refugee_data <- get_cansim_sqlite("98-10-0372-01") %>%
  filter(`Pre-admission experience (7)` == "Total - Pre-admission experience",
         `Admission category and applicant type (7)` == "Refugees",
         `Age (15C)` == "Total - Age",
         `First official language spoken (5)` == "Total - First official language spoken",
         `GEO` == "Canada",
         !`Period of immigration (7)` %in% c(
           "Total - Period of immigration",
           "2011 to 2021"
         ),
         !`Place of birth (290)` %in% c("Outside Canada", 
                                        "Total – Place of birth",
                                        "Ontario",
                                        "Americas",
                                        "North America",
                                        "Central America",
                                        "Caribbean and Bermuda",
                                        "South America",
                                        "Europe",
                                        "Western Europe",
                                        "Eastern Europe",
                                        "Northern Europe",
                                        "Southern Europe",
                                        "Africa",
                                        "Western Africa",
                                        "Eastern Africa",
                                        "Northern Africa",
                                        "Central Africa",
                                        "Southern Africa",
                                        "Asia",
                                        "West Central Asia and the Middle East",
                                        "Eastern Asia",
                                        "Southeast Asia",
                                        "Southern Asia",
                                        "Oceania",
                                        "Antarctica and Adjacent Islands")
  ) %>%
  collect_and_normalize() 

transform_data <- refugee_data %>%
  select(
    Year = `Period of immigration (7)`,
    `Place of birth` = `Place of birth (290)`,
    Number_of_refugees = VALUE
  ) %>%
  mutate(
    `Place of birth` = case_when(
      `Place of birth` == "Inside Canada" ~ "Canada",
      TRUE ~ `Place of birth`
    )
  )

# Get unique places of birth
unique_places <- unique(transform_data$`Place of birth`)

# Initialize vector to store places to keep
places_to_keep <- c()

# Loop through each place of birth
for (place in unique_places) {
  # Get values for this place across all years
  place_values <- transform_data$Number_of_refugees[transform_data$`Place of birth` == place]
  
  # Check if there's at least one non-NA value
  if (any(!is.na(place_values) & place_values != 0)) {
    places_to_keep <- c(places_to_keep, place)
  }
}

# Filter the data to keep only the selected places
filtered_refugee_data <- transform_data %>%
  filter(`Place of birth` %in% places_to_keep) %>%
  mutate(
    # Convert period to end year
    Year = case_when(
      Year == "1980 to 1990" ~ "1990",
      Year == "1991 to 2000" ~ "2000", 
      Year == "2001 to 2010" ~ "2010",
      Year == "2011 to 2015" ~ "2015",
      Year == "2016 to 2021" ~ "2021",
      TRUE ~ Year
    )
  )

canada_refugee_data <- filtered_refugee_data %>%
  filter(
    `Place of birth` == "Canada"
  ) %>%
  inner_join(canada_pop_filtered) %>%
  mutate(
    # Apply the formula: refugees / (population + refugees) * 100,000
    Proportion_refugees = (Number_of_refugees / (Can_Pop + Number_of_refugees)) * 100000
  )

un_refugee_data <- filtered_refugee_data %>%
  filter(
    !`Place of birth` == "Canada"
  ) %>%
  # Convert Year to character to match un_pop_filtered
  mutate(Year = as.character(Year)) %>%
  inner_join(
    un_pop_filtered %>% mutate(Year = as.character(Year)),
    by = c("Year", "Place of birth")
  ) %>%
  mutate(
    # Apply the formula: refugees / (population + refugees) * 100,000
    Proportion_refugees = (Number_of_refugees / (UN_Pop + Number_of_refugees)) * 100000
  )

data_final <-
  bind_rows(canada_refugee_data,un_refugee_data) %>%
  select(
    Year,
    `Country of origin` = `Place of birth`,
    Value = Proportion_refugees
  ) %>%
  mutate(
    # Convert end year to period
    Year = case_when(
      Year == "1990" ~ "1980 to 1990",
      Year == "2000" ~ "1991 to 2000", 
      Year == "2010" ~ "2001 to 2010",
      Year == "2015" ~ "2011 to 2015",
      Year == "2021" ~ "2016 to 2021",
      TRUE ~ Year
    )
  )

write.csv(data_final, "./data/indicator_10-7-4.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")