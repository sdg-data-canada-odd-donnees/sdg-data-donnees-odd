# GIF 12.5.1 --------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)

Raw_data <- get_cansim("38-10-0179-01", factors = FALSE)
Raw_data2 <- get_cansim("17-10-0005-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

# Format the population table to get the per capita
population <-
  Raw_data2 %>%
  filter(REF_DATE >= 2018,
         Gender == "Total - gender",
         `Age group` == "All ages") %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Population = VALUE) %>%
  mutate(Geography = ifelse(
    Geography %in% c("Yukon", "Northwest Territories",
                     "Nunavut"),
    "Territories",
    Geography
  )) %>%
  group_by(Year, Geography) %>%
  summarize(Population = sum(Population , na.rm = TRUE)) %>%
  ungroup()

# Format the waste diversion table and join it the population table
waste_diversion <-
  Raw_data %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of materials diverted`,
    `Sources of materials prepared for recycling`,
    Tonnes = VALUE
  ) %>%
  # convert from metric tonnes to kgs
  mutate(kilos = Tonnes * 1000) %>%
  mutate(
    Geography = recode(
      Geography,
      "Yukon, Northwest Territories and Nunavut" = "Territories")
  ) %>%
  inner_join(population, by = c("Year", "Geography")) %>%
  mutate(Value = kilos / Population) %>%
  select(
    Year,
    Geography,
    `Type of materials diverted`,
    `Sources of materials prepared for recycling`,
    Value
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

# Create the total line
total <-
  waste_diversion %>%
  filter(
    Geography == "Canada",
    `Type of materials diverted` == "All materials diverted",
    `Sources of materials prepared for recycling` == "All sources of diverted materials"
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

non_total <-
  waste_diversion %>%
  filter(
    !(
      Geography == "Canada" &
        `Type of materials diverted` == "All materials diverted" &
        `Sources of materials prepared for recycling` == "All sources of diverted materials"
    )
  )

final_data <-
  bind_rows(total, non_total)

write.csv(final_data,
          "data/indicator_12-5-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
