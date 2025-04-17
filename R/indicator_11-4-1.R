# 11.4.1 ------------------------------------------------------------------

library(cansim)
library(dplyr)
library(stringr)

ccofog <- get_cansim("10-10-0005-01", factors = FALSE)
# local <- get_cansim("10-10-0024-01", factors = FALSE)
pop <- get_cansim("17-10-0005-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

selected_ccofog <- c(
  "Protection of biodiversity and landscape [7054]",
  "Cultural services [7082]"
)

exclude_Canada <- c(
  "Canada"
)

national_provincial <-
  ccofog %>% 
  filter(
    REF_DATE >= 2015,
    `Canadian Classification of Functions of Government (CCOFOG)` %in% selected_ccofog
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Level of government` = `Public sector components`,
    `Type of heritage` = `Canadian Classification of Functions of Government (CCOFOG)`,
    Expenditure = VALUE
  ) %>%
  mutate(
    `Type of heritage` = str_remove(`Type of heritage`, " \\[.*\\]")
  )

pop_filtered <- 
  pop %>% 
  filter(
    REF_DATE >= 2015,
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Population = VALUE
  )

total_cultural_natural <-
  national_provincial %>% 
  group_by(Year, Geography, `Level of government`) %>% 
  summarise(
    `Type of heritage` = "Total cultural and natural heritage", 
    Expenditure = sum(Expenditure, na.rm = TRUE),
    .groups = "drop"
  )

join_total <- bind_rows(total_cultural_natural,heritage_exp) %>% 
  left_join(pop_filtered) %>% 
  transmute(
    Year, Geography, `Level of government`, `Type of heritage`,
    Value = round((Expenditure*1000000)/Population, 5)
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

total_line <-
  join_total %>%
  filter(Geography == "Canada",
         `Level of government` == "Consolidated Canadian general government",
         `Type of heritage` == "Total cultural and natural heritage") %>% 
  mutate_at(2:4, ~ "") %>%
  filter(!is.na(Value)) 

disaggregate <-
  join_total %>%
  filter(
    !(
      Geography == "Canada"&
      `Level of government` == "Consolidated Canadian general government"&
      `Type of heritage` == "Total cultural and natural heritage"
    )
  )

data_final <- bind_rows(total_line, disaggregate) %>%
select(
  Year,
  `Type of heritage`,
  `Level of government`,
  Geography,
  GeoCode,
  Value
)

write.csv(
  data_final,
  "data/indicator_11-4-1.csv",
  row.names = FALSE,
  na = ""
)

