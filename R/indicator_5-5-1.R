# Indicator 5.5.1 ---------------------------------------------------------

# 5.5.1a is federal govenment
# 5.5.1b is local government
# 10-10-0137-01

library(cansim)
library(dplyr)
library(stringr)

# load geocode
geocodes <- read.csv("geocodes.csv")

# 5.5.1(a) ----------------------------------------------------------------

repr_in_gov <- get_cansim("10-10-0137-01", factors = FALSE)

reference_dates <- 
  paste0( 
    c(rep("Members of Parliament on July 1, ", 7), rep("Members of Cabinet on July 1, ", 7)), 
    rep(2015:substr(Sys.Date(), 1, 4), 2)
  )

repr_in_gov <-
  repr_in_gov %>%
  filter(
    `National elected officials` %in% reference_dates,
    Gender %in% c("Women"),
    Statistics == "Proportion"
  ) %>%
  mutate(Government = "National") %>%
  select(
    Geography = GEO,
    Government,
    `Elected officials` = `National elected officials`,
    Value = VALUE
  ) %>% 
  mutate(
    Year = str_sub( `Elected officials`, -4),
    `Elected officials` = str_remove( `Elected officials`, " on July 1, [0-9]{4}")
  ) %>%
  relocate(Year)
  

# 5.5.1(b) ----------------------------------------------------------------

repr_in_first_nations <- get_cansim("41-10-0048-01", factors = FALSE)
  
repr_in_first_nations <- 
  repr_in_first_nations %>%
    filter(
      REF_DATE >= 2015,
      Sex == "Female",
      Statistics == "Proportion"
    ) %>%
    mutate(Government = "Local") %>%
    select(
      Year = REF_DATE,
      Geography = GEO,
      Government,
      `Elected officials` = `First Nation Official`,
      Value = VALUE
    )


# Combine data ------------------------------------------------------------

data_final <- 
  bind_rows(
    repr_in_gov,
    repr_in_first_nations
  ) %>%
  select(
    Year,
    Government,
    `Elected officials`,
    Geography,
    Value
  ) %>%
  mutate(
    Progress = 50 + abs(Value - 50)
  ) %>%
  relocate(Progress, .before = Value) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_5-5-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)