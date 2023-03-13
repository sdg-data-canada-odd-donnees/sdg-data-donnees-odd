# 4.1.2 ------------------------------------------------------------------

library(dplyr)
library(cansim)

completion_rate <- get_cansim("37-10-0130-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

education_lvls <- c(
  "Pre-primary and primary",
  "Lower secondary",
  "Upper secondary and post-secondary non-tertiary",
  "Tertiary education"
)

data_final <-
  completion_rate %>% 
  filter(
    REF_DATE >= 2015,
    GEO != "Organisation for Economic Co-operation and Development (OECD) - average",
    `Educational attainment level` %in% education_lvls
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Educational attainment level`,
    `Age group`,
    Sex,
    Value = VALUE
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "indicator_4-1-2.csv",
  na = "",
  row.names = FALSE
)
  
