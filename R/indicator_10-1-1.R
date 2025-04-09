# Indicator 10.1.1 -------------------------------------------------------------
# Growth rates of household expenditure or income per capita among the bottom
# 40 per cent of the population and the total population

library(dplyr)
library(cansim)
library(tidyr)

# load geocode
geocodes <- read.csv("geocodes.csv")

exp <- get_cansim("11-10-0223-01", factors = FALSE)
income <- get_cansim("11-10-0193-01", factors = FALSE)
pop_raw <- get_cansim("17-10-0005-01", factors = FALSE)

quintiles <- c(
  "All quintiles",
  "Lowest quintile",
  "Second quintile"
)

deciles <- c(
  "Total deciles",
  "Lowest decile",
  "Second decile",
  "Third decile",
  "Fourth decile"
)

cagr <- function(present, past, n) ((present/past)^(1/n)) - 1

cagr_data_transform <- function(data, pop, period) {
  # Inner join population with data and calculate per capita values 
  data <- data %>%
    inner_join(pop) %>% 
    mutate(
      Year = as.numeric(Year),
      Value = Value/Population
    ) %>%
    select(
      Year,
      Geography,
      any_of(c("Household income quintile", "Household income decile")),
      Value
    )
  # Get the earliest and latest years in the data
  yearmin <- min(data$Year)
  yearmax <- max(data$Year)
  # Get a list of missing years in the data between the earliest and latest years
  years_missing <- setdiff(yearmin:yearmax, unique(data$Year))
  # Pivot table so that each year becomes a separate column
  data <- tidyr::pivot_wider(data, names_from = "Year", values_from = "Value")
  # Add columns with NA values for any years that are missing
  if (length(years_missing) > 0) {
    for (yi in years_missing) {
      data[[as.name(yi)]] <- NA
    }
  }
  # Calculate the compound annual growth rate for each year over the given period
  # Add the calculated growth rate values as new columns
  for (yi in yearmin:(yearmax - period)){
    newcolname <- paste(yi, yi + period, sep = "-") # e.g. "2015-2020"
    data[[newcolname]] <- cagr(data[[as.name(yi + period)]], data[[as.name(yi)]], period)
  }
  
  data <- data %>%
    # Remove the columns for per capita values for each year
    select(-any_of(as.character(yearmin:yearmax))) %>%
    # Pivot table back so that growth rate values for each year are in separate rows
    gather(key = "Year", value = "Value", -Geography, -any_of(c("Household income quintile", "Household income decile"))) %>%
    relocate(Year) %>%
    mutate(Value = round(Value*100, 3))
  
}

pop <- pop_raw %>% 
  filter(
    REF_DATE >= 2010,
    # GEO == "Canada",
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Population = VALUE,
  )


# Expenditure per capita ---------------------------------------------------

expenditure_per_cap <-
  exp %>% 
  filter(
    # GEO == "Canada",
    Statistic == "Average expenditure per household",
    `Before-tax household income quintile` %in% quintiles,
    `Household expenditures, summary-level categories` == "Total current consumption"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Household income quintile` = `Before-tax household income quintile`,
    Value = VALUE
  ) %>%
  cagr_data_transform(pop, 5) %>% 
  mutate(
    Series = "Household expenditure per capita"
  )


# Income per capita -------------------------------------------------------

income_per_cap <- 
  income %>% 
  filter(
    REF_DATE >= 2010,
    # GEO == "Canada",
    `Income concept` == "Adjusted after-tax income",
    Statistics == "Average income",
    `Income decile` %in% deciles
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Household income decile` = `Income decile`,
    Value = VALUE
  ) %>% 
 cagr_data_transform(pop, 5) %>% 
 mutate(
    Series = "Income per capita"
  )

data_final <- 
  bind_rows(expenditure_per_cap, income_per_cap) %>% 
  relocate(Series, .after = Year) %>% 
  relocate(`Household income decile`, .after = `Household income quintile`) %>%
  relocate(Geography, .after = `Household income decile`) %>%
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = "Value")

write.csv(
  data_final,
  "data/indicator_10-1-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
