# Indicator 11.2.1 ------------------------------------------------------ 
# Proportion of population that has convenient access to public transport, by
# sex, age and persons with disabilities

library(cansim)
library(dplyr)
library(stringr)

commuting_data <- get_cansim("23-10-0286-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

access_to_public_transp <- 
  commuting_data %>% 
  filter(
    `Demographic, geodemographic and commuting` == "Percentage of population near public transit stop",
    !str_detect(`Hierarchy for GEO`, "[0-9]*\\.[0-9]*\\.[0-9]*")
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>% 
  mutate(
    `Census divisions` = case_when(
      str_detect(Geography, "Census subdivisions in a census agglomeration") ~ "Census subdivisions in a census agglomeration",
      str_detect(Geography, "Area outside census metropolitan areas and census agglomeration") ~ "Area outside census metropolitan areas and census agglomeration",
      TRUE ~ ""
    ),
    Geography = str_remove(Geography, "Area outside census metropolitan areas and census agglomeration, "),
    Geography = str_remove(Geography, "Census subdivisions in a census agglomeration, ")
  ) %>%
  relocate(`Census divisions`, .before = "Value") %>%
  left_join(geocodes) %>%
  mutate(GeoCode = ifelse(`Census divisions` == "", GeoCode, NA)) %>%
  relocate(GeoCode, .before = "Value")

write.csv(
  access_to_public_transp, 
  "data/indicator_11-2-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
  )
