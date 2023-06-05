# 6.3.1 --------------------------------------------------------
library(cansim)
library(dplyr)

industrial_treatment <- get_cansim("38-10-0060-01", factors = FALSE)
municipal_treatment <- get_cansim("38-10-0124-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

industrial_treatment <- 
  industrial_treatment %>% 
  filter(
    REF_DATE >= 2013,
    `North American Industry Classification System (NAICS)` == "Total, all industries"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Type = `Type of final water treatment`,
    Value = VALUE
  ) %>% 
  mutate(
    Treatment = ifelse(
      Type == "Water not treated before discharge",
      "Not treated",
      "Treated"
    )
  ) %>% 
  group_by(Year, Geography, Treatment) %>% 
  summarise(Value = sum(Value)) %>% 
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = Value
  ) %>% 
  mutate(
    Total = `Not treated` + Treated
  ) %>% 
  transmute(
    Series = "Wastewater discharge from manufacturing industries",
    Value = (Treated/Total)*100
  )

municipal_treatment <- 
  municipal_treatment %>% 
  filter(
    REF_DATE >= 2013
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Type = `Volume by treatment type`,
    Value = VALUE
  ) %>% 
  mutate(
    Treatment = ifelse(
      Type == "No treatment",
      "Not treated",
      "Treated"
    )
  ) %>% 
  group_by(Year, Geography, Treatment) %>% 
  summarise(Value = sum(Value)) %>% 
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = Value
  ) %>% 
  mutate(
    Total = `Not treated` + Treated
  ) %>% 
  transmute(
    Series = "Wastewater discharged from municipal sewage systems",
    Value = (Treated/Total)*100
  )


data_final <- 
  bind_rows(industrial_treatment, municipal_treatment) %>% 
    left_join(geocodes) %>% 
    mutate(Geography = ifelse(Geography == "Canada", "", Geography)) %>% 
    relocate(Series, .after = Year) %>% 
    relocate(GeoCode, .before = Value)


write.csv(
  data_final,
  "data/indicator_6-3-1.csv",
  row.names = FALSE,
  na = ""
)  
