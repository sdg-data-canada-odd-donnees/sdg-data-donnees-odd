
# 9.1.2 -------------------------------------------------------------------

library(dplyr)
library(cansim)


# load data --------
air_passengers <- get_cansim("23-10-0253-01", factors = FALSE)
air_freight <- get_cansim("23-10-0254-01", factors = FALSE)

rail_passengers_domestic <- get_cansim("23-10-0057-01", factors = FALSE)
rail_freight <- get_cansim("23-10-0062-01", factors = FALSE)

trucking_freight <- get_cansim("23-10-0219-01", factors = FALSE)


# air ---------------------------------------------------------------------
air_passengers <- 
  air_passengers %>% 
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    `Air passenger traffic` == "Total, passengers enplaned and deplaned"
  ) %>% 
  select(
    Year = REF_DATE,
    Value = VALUE
  ) %>% 
  mutate(
    Series = "Air passengers",
    Units = "Number of passengers"
  )

air_freight <- 
  air_freight %>% 
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    `Air cargo traffic` == "Total, cargo loaded and unloaded"
  ) %>% 
  select(
    Year = REF_DATE,
    Value = VALUE
  ) %>% 
  mutate(
    Series = "Air freight volume",
    Units = "Metric tonnes"
  )


# rail --------------------------------------------------------------------

rail_passengers_domestic <- 
  rail_passengers_domestic %>% 
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    Companies == "Total companies",
    Statistics == "Revenue passengers, passengers"
  ) %>% 
  select(
    Year = REF_DATE,
    Value = VALUE
  ) %>% 
  mutate(
    Series = "Rail passengers",
    Units = "Number of passengers",
    Value = Value * 1000
  )

rail_freight <- 
  rail_freight %>%
  filter(
    REF_DATE >= 2015,
    GEO == "Total tonnage from all origins",
    `Geography, destination of commodities` == "Total tonnage for all destinations",
    `Commodities and commodity groups` == "Total tonnage of all rail commodities"
  ) %>% 
  select(
    Year = REF_DATE,
    Value = VALUE
  ) %>% 
  mutate(
    Series = "Rail freight volume",
    Units = "Metric tonnes"
  )


# trucking ----------------------------------------------------------------

trucking_freight <-
  trucking_freight %>% 
  filter(
    REF_DATE >= 2015,
    `Shipment type` == "All shipments",
    `Trucking industry activity` == "Weight"
  ) %>% 
  select(
    Year = REF_DATE,
    Value = VALUE
  ) %>% 
  mutate(
    Series = "Road freight volume",
    Units = "Metric tonnes of cargo",
    Value = Value * 0.001
  )

data_final <-
  bind_rows(
    air_freight,
    air_passengers,
    rail_freight,
    rail_passengers_domestic,
    trucking_freight
  ) %>% 
  relocate(Value, .after = last_col()) 

write.csv(
  data_final,
  "data/indicator_9-1-2.csv",
  na = "",
  row.names = FALSE
)
