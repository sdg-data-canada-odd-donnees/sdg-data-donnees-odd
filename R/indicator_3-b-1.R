## GIF 3.b.1: Proportion of the target population covered by all vaccines included in their national programme

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

vaccine_coverage <- get_cansim("13-10-0870-01", factors = FALSE)

# Series
# Proportion of the target population who received 3 doses of diphtheria-tetanus-pertussis vaccine
# - Proxy: data available for 2-year-olds having received 4 doses of DPT vaccine (use diphtheria for progress measurement)
# Proportion of the target population who received 2 doses of measles vaccine
# - Data available for 7-year-olds having received 2 doses of measles vaccine
# Proportion of the target population who received 3 doses of pneumococcal conjugate vaccine
# - Data available for 2-year-olds having received 3-4 doses of pneumococcal vaccine (4 doses only in NWT and NU, 3 doses in provinces + Yukon)
# Proportion of the target population who received the final dose of HPV vaccine
# - Proxy: Data available for 14-year-olds having receiving the first dose of HPV vaccine

data_prep <- vaccine_coverage %>%
  filter(
    REF_DATE >= 2015,
    Characteristics == "Percentage vaccinated",
  ) %>%
  select(
    Year = REF_DATE,
    `Antigen or vaccine`,
    `Target population`,
    Geography = GEO,
    Gender = Sex,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  na.omit() %>%
  mutate(
    GeoCode = as.integer(GeoCode)
  )

# DPT3
DTP <- data_prep %>%
  filter(
    `Antigen or vaccine` %in% c("Diphtheria", "Tetanus", "Pertussis (whooping cough)"),
    `Target population` == "Recommended vaccines for 2-year-old children"
  ) %>%
  transmute(
    Year,
    Series = "Diphtheria-tetanus-pertussis vaccine coverage at 2 years of age",
    `Antigen or vaccine`,
    Geography,
    # Gender,
    GeoCode,
    Value
  )

# Pneumococcal
pneumococcal <- data_prep %>%
  filter(
    `Antigen or vaccine` == "Pneumococcal",
    `Target population` == "Recommended vaccines for 2-year-old children"
  ) %>%
  transmute(
    Year,
    Series = "Pneumococcal vaccine coverage at 2 years of age",
    Geography = case_match(Geography, "Canada" ~ NA, .default = Geography),
    # Gender,
    GeoCode,
    Value
  )

# Measles
measles <- data_prep %>%
  filter(
    `Antigen or vaccine` == "Measles",
    `Target population` == "Recommended vaccines for 7-year-old children"
  ) %>%
  transmute(
    Year,
    Series = "Measles vaccine coverage at 7 years of age",
    Geography = case_match(Geography, "Canada" ~ NA, .default = Geography),
    # Gender,
    GeoCode,
    Value
  )

# HPV
HPV <- data_prep %>%
  filter(
    `Antigen or vaccine` == "Human Papillomavirus (HPV)",
    `Target population` == "Recommended vaccines for 14-year-old children"
  ) %>%
  transmute(
    Year,
    Series = "Human Papillomavirus (HPV) vaccine coverage at 14 years of age",
    across(
      c(Geography, Gender),
      ~ replace(., Geography == "Canada" & Gender == "Total - Gender", NA)
    ),
    GeoCode,
    Value
  )

data_final <- bind_rows(DTP, measles, pneumococcal, HPV) %>%
  relocate(Gender, .before = GeoCode)

write.csv(
  data_final,
  "data/indicator_3-b-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
