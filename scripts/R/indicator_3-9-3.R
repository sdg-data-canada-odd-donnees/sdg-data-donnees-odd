
# 3.9.3 -------------------------------------------------------------------

library(dplyr)
library(cansim)

causes_poison <- c(
  "Accidental poisoning by and exposure to nonopioid analgesics, antipyretics and antirheumatics",
  "Accidental poisoning by and exposure to other drugs acting on the autonomic nervous system",
  "Accidental poisoning by and exposure to organic solvents and halogenated hydrocarbons and their vapours",
  "Accidental poisoning by and exposure to carbon monoxide and other gases and vapours",
  "Accidental poisoning by and exposure to pesticides",
  "Accidental poisoning by and exposure to other and unspecified chemicals and noxious substances"
)

poisonings <- get_cansim("13-10-0156-01", factors = FALSE) 


pop <- 
  get_cansim("17-10-0005-01", factors = FALSE) %>% 
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    Sex == "Both sexes",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Population = VALUE
  )

data_final <- 
  poisonings %>% 
  mutate(
    `Cause of death (ICD-10)` = str_remove(`Cause of death (ICD-10)`, " \\[.*\\]")
  ) %>% 
  filter(
    REF_DATE >= 2015,
    `Age group` == "Total, all ages",
    Sex == "Both sexes",
    `Cause of death (ICD-10)` %in% causes_poison
  ) %>% 
  select(
    Year = REF_DATE,
    `Cause of death (ICD-10)`,
    Value = VALUE
  ) %>% 
  group_by(Year) %>% 
  summarise(Deaths = sum(Value, na.rm = TRUE), .groups = "keep") %>% 
  left_join(pop) %>% 
  transmute(Value = round((Deaths/Population)*100000, 3))


write.csv(
  data_final,
  "data/indicator_3-9-3.csv",
  na = "",
  row.names = FALSE
)
