# Indicator 3.4.1 ---------------------------------------------------------
# Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease

library(cansim)
library(dplyr)
library(stringr)

# get data from stc api
mortality_data <- get_cansim("13-10-0394-01", factors = FALSE)

# diseases considered in this indicator
diseases <- c(
  "Malignant neoplasms [C00-C97]",
  "Diabetes mellitus [E10-E14]",
  "Diseases of heart [I00-I09, I11, I13, I20-I51]",
  "Essential hypertension and hypertensive renal disease [I10, I12, I15]",
  "Cerebrovascular diseases [I60-I69]",
  "Atherosclerosis [I70]",
  "Aortic aneurysm and dissection [I71]",
  "Chronic lower respiratory diseases [J40-J47]",
  "Pneumoconioses and chemical effects [J60-J66, J68]",
  "Pneumonitis due to solids and liquids [J69]"
)

data_final <- 
  mortality_data %>%
  filter(
    REF_DATE >= 2015,
    Characteristics == "Age-specific mortality rate per 100,000 population",
    `Leading causes of death (ICD-10)` %in% diseases 
  ) %>% 
  mutate(
    `Age at time of death` = str_remove(`Age at time of death`, "Age at time of death, "),
    `Age at time of death` = str_to_sentence(`Age at time of death`),
    `Leading causes of death (ICD-10)` = str_remove(`Leading causes of death (ICD-10)`, " \\[.*\\]")
  ) %>%
  select(
    Year = REF_DATE, 
    `Age at time of death`, 
    Sex, 
    `Causes of death` = `Leading causes of death (ICD-10)`, 
    Value = VALUE
  )

write.csv(
  data_final,
  "data/indicator_3-4-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
