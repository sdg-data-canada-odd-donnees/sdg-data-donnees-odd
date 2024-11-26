# Indicator 11.6.1 ------------------------------------------------------ 
# Total amount of waste sent to disposal via the waste management industry

library(cansim)
library(dplyr)

waste_data <- get_cansim("38-10-0032-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

waste_disposal <- 
  waste_data %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Sources of waste for disposal`,
    Value = VALUE
  ) %>%
  left_join(geocodes) %>% 
  mutate(
    GeoCode = ifelse(`Sources of waste for disposal` == "All sources of waste for disposal", GeoCode, NA)
  ) %>% 
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    waste_disposal %>%
      filter(Geography == "Canada", `Sources of waste for disposal` == "All sources of waste for disposal") %>%
      mutate(across(2:(ncol(.)-2), ~ NA)),
    waste_disposal %>%
      filter(
        !(Geography == "Canada" & `Sources of waste for disposal` == "All sources of waste for disposal")
      )
  )

write.csv(
  data_final, 
  "data/indicator_11-6-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)  
