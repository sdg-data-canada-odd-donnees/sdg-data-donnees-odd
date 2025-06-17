# Indicator 3.9.2 ---------------------------------------------------------
# Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (exposure to unsafe Water, Sanitation and Hygiene for All (WASH) services)

library(cansim)
library(dplyr)
library(stringr)

diseases_A00_to_B99 <- get_cansim("13-10-0141-01", factors = FALSE)
diseases_E00_to_E90 <- get_cansim("13-10-0144-01", factors = FALSE)
diseases_H60_to_H95 <- get_cansim("13-10-0780-01", factors = FALSE)
diseases_J00_to_J99 <- get_cansim("13-10-0782-01", factors = FALSE)
diseases_P00_to_P96 <- get_cansim("13-10-0153-01", factors = FALSE)
diseases_U00_to_U99 <- get_cansim("13-10-0391-01", factors = FALSE)
population <- get_cansim("17-10-0005-01", factors = FALSE)

diarrhea <- c(
  "Typhoid and paratyphoid fevers [A01]",
  "Shigellosis [A03]",
  "Other bacterial intestinal infections [A04]",
  "Amoebiasis [A06]",
  "Other protozoal intestinal diseases [A07]",
  "Viral and other specified intestinal infections [A08]",
  "Other gastroenteritis and colitis of infectious and unspecified origin [A09]"
)

respiratory <- c(
  "Nonsuppurative otitis media [H65]",
  "Suppurative and unspecified otitis media [H66]",
  "Acute upper respiratory infections [J00-J06]",
  "Influenza and pneumonia [J09-J18]",
  "Other acute lower respiratory infections [J20-J22]",
  "Congenital pneumonia [P23]",
  "Severe acute respiratory syndrome [SARS] [U04]"
)

# B76 and B79 are not in the tables
intestinal <- c(
  "Ascariasis [B77]"
)

protein <- c(
  "Malnutrition [E40-E46]"
)

population_data <-
  population %>%
  filter(
    REF_DATE >= 2015,
    Gender == "Total - gender",
    `Age group` == "All ages",
    GEO == "Canada"
  ) %>%
  select(
    Year = REF_DATE,
    Population_Count = VALUE
  )

diarrhea_data <-
  diseases_A00_to_B99 %>%
  filter(
    REF_DATE >= 2015,
    Sex == "Both sexes",
    `Age group` == "Total, all ages",
    `Cause of death (ICD-10)` %in% diarrhea
  ) %>%
  select(
    Year = REF_DATE,
    `Type of disease` = `Cause of death (ICD-10)`,
    Count = VALUE
  ) %>%
  mutate(`Type of disease` = "Diarrhea") %>% # Assign a common name
  group_by(Year, `Type of disease`) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop") # Sum counts

diarrhea_by_population <-
  left_join(diarrhea_data, population_data) %>%
  mutate(Value = (Total_Count / Population_Count) * 100000) %>%
  select(
    Year,
    `Type of disease`,
    Value
  )

respiratory_data <-
  bind_rows(diseases_H60_to_H95,diseases_J00_to_J99, diseases_P00_to_P96,diseases_U00_to_U99) %>%
  filter(
    REF_DATE >= 2015,
    Sex == "Both sexes",
    `Age group` == "Total, all ages",
    `Cause of death (ICD-10)` %in% respiratory
  ) %>%
  select(
    Year = REF_DATE,
    `Type of disease` = `Cause of death (ICD-10)`,
    Count = VALUE
  ) %>%
  mutate(`Type of disease` = "Acute respiratory infections") %>% # Assign a common name
  group_by(Year, `Type of disease`) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")

respiratory_by_population <-
  left_join(respiratory_data, population_data) %>%
  mutate(Value = (Total_Count / Population_Count) * 100000) %>%
  select(
    Year,
    `Type of disease`,
    Value
  )

intestinal_data <-
  diseases_A00_to_B99 %>%
  filter(
    REF_DATE >= 2015,
    Sex == "Both sexes",
    `Age group` == "Total, all ages",
    `Cause of death (ICD-10)` %in% intestinal
  ) %>%
  select(
    Year = REF_DATE,
    `Type of disease` = `Cause of death (ICD-10)`,
    Count = VALUE
  ) %>%
  mutate(`Type of disease` = "Intestinal nematode infections") %>% # Assign a common name
  group_by(Year, `Type of disease`) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")

intestinal_by_population <-
  left_join(intestinal_data, population_data) %>%
  mutate(Value = (Total_Count / Population_Count) * 100000) %>%
  select(
    Year,
    `Type of disease`,
    Value
  )

protein_data <-
  diseases_E00_to_E90 %>%
  filter(
    REF_DATE >= 2015,
    Sex == "Both sexes",
    `Age group` == "Total, all ages",
    `Cause of death (ICD-10)` %in% protein
  ) %>%
  select(
    Year = REF_DATE,
    `Type of disease` = `Cause of death (ICD-10)`,
    Count = VALUE
  ) %>%
  mutate(`Type of disease` = "Protein-energy malnutrition") %>% # Assign a common name
  group_by(Year, `Type of disease`) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")

protein_by_population <-
  left_join(protein_data, population_data) %>%
  mutate(Value = (Total_Count / Population_Count) * 100000) %>%
  select(
    Year,
    `Type of disease`,
    Value
  )

total_by_population <- 
  bind_rows(
    respiratory_by_population,
    diarrhea_by_population,
    intestinal_by_population,
    protein_by_population
  ) %>%
  summarise(Value = sum(Value), .by = Year)

data_final <-
  bind_rows(
    total_by_population,
    respiratory_by_population,
    diarrhea_by_population,
    intestinal_by_population,
    protein_by_population
  ) %>%
  mutate(Value = round(Value, 2)) %>%
  select(
    Year,
    `Type of disease`,
    Value
  )

write.csv(
  data_final,
  "data/indicator_3-9-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
