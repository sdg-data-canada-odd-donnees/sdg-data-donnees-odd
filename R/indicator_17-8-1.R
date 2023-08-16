# Indicator 17.8.1 ---------------------------------------------------------
# Proportion of individuals using the Internet

library(cansim)
library(dplyr)

internet_data <- get_cansim("22-10-0135-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

internet_use <- 
  internet_data %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Age group`,
    Value = VALUE
  ) %>%
  left_join(geocodes) %>%
  mutate(
    GeoCode = ifelse(`Age group` != "Total, 15 years and over", NA, GeoCode)
  ) %>%
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    internet_use %>%
      filter(Geography == "Canada", `Age group` == "Total, 15 years and over") %>%
      mutate(across(Geography:`Age group`, ~ "")),
    internet_use %>%
      filter(!(Geography == "Canada" & `Age group` == "Total, 15 years and over"))
  )

write.csv(
  data_final, 
  "data/indicator_17-8-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
