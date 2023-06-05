# Indicator 3.5.2 ---------------------------------------------------------
# Alcohol per capita consumption

library(cansim)
library(dplyr)

alc_sales <- get_cansim("10-10-0010-01", factors = FALSE)

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
    Date = REF_DATE,
    Geography = GEO,
    Value = VALUE
  )

total_line <- 
  alc_sales_total %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(Geography = "")  

data_final <- bind_rows(total_line, alc_sales_total)  

write.csv(
  data_final,
  "data/indicator_3-5-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

