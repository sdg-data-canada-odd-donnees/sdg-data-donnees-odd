# Indicator 3.5.2 ---------------------------------------------------------
# Alcohol per capita consumption

library(cansim)
library(dplyr)

alc_sales <- get_cansim("10-10-0010-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

exclude_Canada <- c(
  "Canada"
)

alc_sales_total <-
  alc_sales %>%
  filter(
    substr(REF_DATE, 1, 4) >= 2014,
    UOM == "Litres",
    `Type of sales` == "Total per capita sales",
    `Type of beverage` == "Total alcoholic beverages",
    `Value, volume and absolute volume` == "Absolute volume for total per capita sales"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

total_line <- 
  alc_sales_total %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(Geography = "")  

data_with_Canada <- bind_rows(total_line, alc_sales_total) 

data_final <-
  data_with_Canada %>%
  filter(
    !Geography %in% exclude_Canada
  )

write.csv(
  data_final,
  "data/indicator_3-5-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

