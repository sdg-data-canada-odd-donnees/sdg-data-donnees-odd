# Indicator 9.5.1 ------------------------------------------------------
# 9.5.1 Research and development expenditure as a proportion of GDP

library(cansim)
library(dplyr)

rd_gdp <- get_cansim("27-10-0359-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

exclude_geos <- c(
  "France",
  "Germany",
  "Italy",
  "Japan",
  "United Kingdom",
  "United States"
)

data_final <-
  rd_gdp %>%
  filter(
    REF_DATE >= 2015,
    !GEO %in% exclude_geos
  )%>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>% 
  mutate(
    Geography = replace(Geography, Geography == "Canada", NA)
  )%>%
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

write.csv(
  data_final, 
  "data/indicator_9-5-1.csv", 
  na = "", 
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
