# 9.2.1 ------------------------------------------------------------------
# Manufacturing value added as a proportion of GDP and per capita

library(cansim)
library(dplyr)

mva <- get_cansim("16-10-0117-01", factors = FALSE)
gdp <- get_cansim("36-10-0222-01", factors = FALSE)
pop <- get_cansim("17-10-0005-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

mva <- 
  mva %>%
  filter(
    REF_DATE >= 2015,
    `Principal statistics` == "Manufacturing value added",
    `North American Industry Classification System (NAICS)` == "Manufacturing [31-33]"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    mva = VALUE
  ) 

gdp <- 
  gdp %>% 
  filter(
    REF_DATE >= 2015,
    Prices == "Current prices",
    Estimates == "Gross domestic product at market prices"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    gdp = VALUE
  )

pop <- 
  pop %>% 
  filter(
    REF_DATE >= 2015,
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    pop = VALUE
  )

data_final <-
  bind_rows(
    mva %>% 
      inner_join(gdp) %>% 
      filter(!is.na(gdp)) %>%
      mutate(
        Series = "MVA as a proportion of GDP",
        Units = "Percentage (%)",
        Value = round(mva*1000/(gdp*1000000)*100, 2),
      ) %>%
      select(-mva, -gdp),
    mva %>% 
      inner_join(pop) %>%
      filter(!is.na(pop)) %>%
      mutate(
        Series = "MVA per capita",
        Units = "Current dollars",
        Value = round(mva*1000/pop, 2),
      ) %>%
      select(-mva, -pop)
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = "Value") %>%
  relocate(Geography, .after = "Units") %>%
  mutate(Geography = ifelse(Geography == "Canada", NA, Geography))

write.csv(
  data_final,
  "data/indicator_9-2-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
