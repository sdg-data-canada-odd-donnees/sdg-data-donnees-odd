# Indicator 3.c.2 ---------------------------------------------------------
# Health worker density and distribution

library(cansim)
library(dplyr)
library(stringr)

print(sessionInfo())

health_care_workers <- c(
  "31101 Specialists in surgery",
  "31102 General practitioners and family physicians",
  "31301 Registered nurses and registered psychiatric nurses",
  "31302 Nurse practitioners",
  "31303 Physician assistants, midwives and allied health professionals",
  "31110 Dentists",
  "31120 Pharmacists"
)

medical_doctors <- c(
  "31101 Specialists in surgery",
  "31102 General practitioners and family physicians"
)

nursing <- c(
  "31301 Registered nurses and registered psychiatric nurses",
  "31302 Nurse practitioners",
  "31303 Physician assistants, midwives and allied health professionals"
)

dentists <- c(
  "31110 Dentists"
)

pharmacists <- c(
  "31120 Pharmacists"
)

population <- get_cansim("17-10-0005-01", factors = FALSE)

connection <- get_cansim_sqlite("98-10-0447-01")

health_density <- connection %>%
  filter(`Age (15A)` == "Total - Age",
         `Major field of study - Classification of Instructional Programs (CIP) 2021 (63)` == "Total - Major field of study - Classification of Instructional Programs (CIP) 2021",
         `Statistics (3)` == "Count",
         `Occupation - Unit group - National Occupational Classification (NOC) 2021 (821A)` %in% health_care_workers,
         `Highest certificate, diploma or degree (7)` == "Total - Highest certificate, diploma or degree"
  ) %>%
  collect_and_normalize()

disconnect_cansim_sqlite(connection)

health_density_filtered <-
  health_density %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Gender = `Gender (3)`,
         Occupation = `Occupation - Unit group - National Occupational Classification (NOC) 2021 (821A)`,
         GeoCode = GeoUID,
         Count = VALUE,
  )

population_data <-
  population %>%
  filter(
    REF_DATE >= 2021,
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>%
  select(
    Year = REF_DATE,
    Population_Count = VALUE
  )

# First Series: Health worker density
health_worker_density <- health_density_filtered %>%
  filter(Gender == "Total - Gender") %>%
  mutate(
    `Type of occupation` = case_when(
      Occupation %in% medical_doctors ~ "Density of medical doctors",
      Occupation %in% nursing ~ "Density of nursing and midwifery personnel",
      Occupation %in% dentists ~ "Density of dentists",
      Occupation %in% pharmacists ~ "Density of pharmacists"
    )
  ) %>%
  group_by(Year, Geography, `Type of occupation`) %>%
  summarize(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  left_join(population_data, by = "Year") %>%
  mutate(
    Value = (Total_Count / Population_Count) * 10000,
    Series = "Health worker density, by type of occupation (per 10,000 population)",
    Units = "Per 10,000 population"
  ) %>%
  select(Year, Geography, Series, Units, `Type of occupation`, Value)

# Second Series: Health worker distribution by sex
health_worker_distribution <- health_density_filtered %>%
  filter(Occupation %in% c(medical_doctors, nursing)) %>%
  group_by(Year, Geography, Occupation, Gender) %>%
  summarize(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year, Geography, Occupation) %>%
  mutate(
    Total_Gender_Count = sum(Total_Count[Gender == "Total - Gender"], na.rm = TRUE),
    Percentage = (Total_Count / Total_Gender_Count) * 100
  ) %>%
  ungroup() %>%
  filter(Gender %in% c("Male", "Female")) %>%
  mutate(
    `Type of occupation` = case_when(
      Occupation %in% medical_doctors ~ "Medical doctors",
      Occupation %in% nursing ~ "Nursing and midwifery personnel"
    ),
    Series = "Health worker distribution, by sex and type of occupation",
    Units = "Percentage (%)"
  ) %>%
  select(Year, Geography, Series, Units, `Type of occupation`, Gender, Value = Percentage)

data_final <-
  bind_rows(health_worker_density, health_worker_distribution)

write.csv(
  data_final,
  "data/indicator_3-c-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
  

