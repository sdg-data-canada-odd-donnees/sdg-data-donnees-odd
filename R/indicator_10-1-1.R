# 10.1.1 ------------------------------------------------------------------

library(dplyr)
library(cansim)
library(tidyr)

exp <- get_cansim("11-10-0223-01", factors = FALSE)
income <- get_cansim("11-10-0193-01", factors = FALSE)
pop <- get_cansim("17-10-0005-01", factors = FALSE)

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

cagr_data_transform <- function(data) {
  
  data %>% 
    left_join(pop) %>% 
    transmute(
      Year,
      Geography,
      `Household income quintile or decile`,
      Value = Value/Population
    ) %>% 
    tidyr::pivot_wider(
      names_from = "Year",
      values_from = "Value"
    ) %>%
    transmute(
      Geography,
      `Household income quintile or decile`,
      `2010-2015` = cagr(`2015`, `2010`, 5),
      `2011-2016` = cagr(`2016`, `2011`, 5),
      `2012-2017` = cagr(`2017`, `2012`, 5),
      `2014-2019` = cagr(`2019`, `2014`, 5)
    ) %>% 
    tidyr::pivot_longer(
      cols = 3:6,
      names_to = "Year",
      values_to = "Value"
    ) %>% 
    relocate(Year) %>% 
    mutate(
      Value = round(Value*100, 3)
    )
  
}

pop <- 
  pop %>% 
  filter(
    REF_DATE >= 2010,
    GEO == "Canada",
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Population = VALUE
  )


# Expenditure per capita ---------------------------------------------------

expenditure_per_cap <-
  exp %>% 
  filter(
    GEO == "Canada",
    Statistic == "Average expenditure per household",
    `Before-tax household income quintile` %in% quintiles,
    `Household expenditures, summary-level categories` == "Total current consumption"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Household income quintile or decile` = `Before-tax household income quintile`,
    Value = VALUE
  ) %>%
  cagr_data_transform() %>% 
  mutate(
    Series = "Household expenditure per capita"
  )


# Income per capita -------------------------------------------------------

income_per_cap <- 
  income %>% 
  filter(
    REF_DATE >= 2010,
    GEO == "Canada",
    `Income concept` == "Adjusted after-tax income",
    Statistics == "Average income",
    `Income decile` %in% deciles
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Household income quintile or decile` = `Income decile`,
    Value = VALUE
  ) %>% 
 cagr_data_transform() %>% 
 mutate(
    Series = "Income per capita"
  )

data_final <- 
  bind_rows(expenditure_per_cap, income_per_cap) %>% 
  relocate(Series, .after = Year) %>% 
  select(-Geography) # temp

write.csv(
  data_final,
  "data/indicator_10-1-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
