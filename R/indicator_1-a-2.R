# 1.a.2 ------------------------------------------------------------------
# Proportion of total government spending on essential services (education, health and social protection)

library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

raw_data <- get_cansim("10-10-0005-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

essential_services <- 
  raw_data %>%
  filter(
    REF_DATE >= 2015,
    `Public sector components` == "Consolidated Canadian general government",
    GEO=="Canada",
    `Canadian Classification of Functions of Government (CCOFOG)` %in% c("General public services [701]",  #le == ne fonctionnait pas ici étrangement, donc faire %in% lorsque c'est des listes?
    "Defence [702]",
    "Public order and safety [703]",
    "Economic affairs [704]",
    "Environmental protection [705]",
    "Housing and community amenities [706]",
    "Health [707]",
    "Recreation, culture and religion [708]",
    "Education [709]",
    "Social protection [710]")
  )  %>% 
  select(
    Year = REF_DATE,
    Value = VALUE,
    Function_of_gov = `Canadian Classification of Functions of Government (CCOFOG)`
  ) %>%
  pivot_wider(names_from = "Function_of_gov", values_from = "Value") %>%
 mutate(
    prop_value=
    round((((`Health [707]`+ `Education [709]`+ `Social protection [710]`) * 100) / 
    (`General public services [701]`+
    `Defence [702]`+
    `Public order and safety [703]`+
    `Economic affairs [704]`+
    `Environmental protection [705]`+
    `Housing and community amenities [706]`+
    `Health [707]`+
    `Recreation, culture and religion [708]`+
    `Education [709]`+
    `Social protection [710]`)), digits = 2)) %>%
 select(
    Year = Year,
    Value = prop_value
  )

write.csv(
  essential_services,
  "data/indicator_1-a-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
  )
