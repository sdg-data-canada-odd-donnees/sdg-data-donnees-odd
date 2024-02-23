# 6.3.1 --------------------------------------------------------
library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

industrial_treatment_data <- get_cansim("38-10-0060-01", factors = FALSE)
municipal_treatment_data <- get_cansim("38-10-0124-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

exclude_Canada <- c(
  "Canada"
)

exclude_dimension <- c(
  "All treatment types"
)

exclude_treatment <- c(
  "Primary treatment",
  "Secondary treatment",
  "Secondary treatment with additional phosphorus removal",
  "Tertiary treatment"
)

industrial_treatment <- 
  industrial_treatment_data %>% 
  filter(
    REF_DATE >= 2013,
    `North American Industry Classification System (NAICS)` == "Total, all industries"
  ) %>% 
#  mutate(
#    `North American Industry Classification System (NAICS)` = str_remove(`North American Industry Classification System (NAICS)`, " \\[.*\\]")
#  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
#    `Manufacturing industries` = `North American Industry Classification System (NAICS)`,
    Type = `Type of final water treatment`,
    Value = VALUE
  ) %>% 
  mutate(
    Treatment = ifelse(
      Type == "Water not treated before discharge",
      "Not treated",
      "Treated")
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
  municipal_treatment_data %>% 
  filter(
    REF_DATE >= 2013,
    !`Volume by treatment type` %in% exclude_treatment
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
      "All treatment"
    )
  )%>% 
  group_by(Year, Geography, Treatment) %>% 
  summarise(Value = sum(Value)) %>% 
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = Value
  ) %>% 
  mutate(
    Treated = `All treatment` - `Not treated`
  ) %>% 
  transmute(
    Series = "Wastewater discharged from municipal sewage systems",
    Value = round((Treated/`All treatment`)*100, 2)
  )

municipal_treatment_comparison <- 
  municipal_treatment_data %>% 
  filter(
    REF_DATE >= 2013,
    !`Volume by treatment type` %in% exclude_dimension
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
    Value = round((Treated/Total)*100, 2)
  )

data_merge <- 
  bind_rows(industrial_treatment, municipal_treatment) %>%
  left_join(geocodes) %>% 
  relocate(Series, .after = Year) %>% 
  relocate(GeoCode, .before = Value)

total_line <-
  data_merge %>% 
    filter(
    Geography == "Canada"
  ) %>%
  mutate(Geography = "")

data_with_Canada <- bind_rows(total_line, data_merge)

data_final <-
  data_with_Canada %>%
  filter(
    !Geography %in% exclude_Canada
  ) 

write.csv(
  data_final,
  "data/indicator_6-3-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)  
