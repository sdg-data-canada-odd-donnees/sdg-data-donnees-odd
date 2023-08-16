
# 4.3.1 -------------------------------------------------------------------

library(cansim)
library(dplyr)

participation_youth <- get_cansim("37-10-0102-01", factors = FALSE)
participation_adult <- get_cansim("37-10-0103-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

participation_youth <- 
  participation_youth %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2014
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Age group`,
    `Type of institution attended`,
    Value = VALUE
  ) %>% 
  mutate(
    `Type of institution attended` = ifelse(
      `Type of institution attended` == "Total, type of institution attended", 
      "Total participation rate", 
      `Type of institution attended`
    )
  )

participation_adult <- 
  participation_adult %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2014,
    `Age group` == "30 to 34 years"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Age group`,
    `Type of institution attended` = `Participation rate by type of institution attended`,
    Value = VALUE
  )

data_final <- 
  bind_rows(participation_youth, participation_adult) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_4-3-1.csv",
  row.names = FALSE,
  na = '',
  fileEncoding = "UTF-8"
)
