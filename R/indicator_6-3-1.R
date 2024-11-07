# 6.3.1 --------------------------------------------------------
# Proportion of domestic and industrial wastewater flows safely treated

library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

industrial_treatment_data <- get_cansim("38-10-0060-01", factors = FALSE)
municipal_treatment_data <- get_cansim("38-10-0124-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")


industrial_treatment <- industrial_treatment_data %>%
  filter(
    REF_DATE >= 2013,
    GEO == "Canada",
    `North American Industry Classification System (NAICS)` == "Total, all industries"
  ) %>%
  mutate(
    Units = "Million cubic metres",
    Source = "Industries",
    `Wastewater treatment level` = case_match(
      `Type of final water treatment`,
      c("Secondary or biological water treatment", "Tertiary or advanced water treatment") ~ "Safely treated",
      "Primary or mechanical water treatment" ~ "Treated",
      "Water not treated before discharge" ~ "Not treated"
    )
  ) %>%
  select(
    Year = REF_DATE,
    Units,
    # Geography = GEO,
    Source,
    `Wastewater treatment level`,
    Value = VALUE
  ) %>%
  # Combine the safely treated values together into a single entry for each year
  group_by(Year, Units, Source, `Wastewater treatment level`) %>%
  summarise(Value = sum(Value)) %>%
  # Widen the treatment level column into separate columns for each treatment type for the following operations
  pivot_wider(
    names_from = `Wastewater treatment level`,
    values_from = Value
  ) %>%
  mutate(
    Treated = Treated + `Safely treated`,
    Generated = Treated + `Not treated`
  )

# Calculate proportions of industrial wastewater safely treated/treated/not treated.
industrial_porportions <- industrial_treatment %>%
  mutate(
    Units = "Percentage (%)",
    `Safely treated` = `Safely treated`/Generated*100,
    Treated = Treated/Generated*100,
    `Not treated` = `Not treated`/Generated*100,
  ) %>%
  select(-Generated)


municipal_treatment <- municipal_treatment_data %>%
  filter(
    REF_DATE >= 2013,
    # `Volume by treatment type` != "All treatment types",
    GEO == "Canada"
  ) %>% 
  mutate(
    Units = "Million cubic metres",
    Source = "Municipal",
    `Wastewater treatment level` = case_match(
      `Volume by treatment type`,
      c("Secondary treatment", "Secondary treatment with additional phosphorus removal", "Tertiary treatment") ~ "Safely treated",
      "Primary treatment" ~ "Treated",
      "No treatment" ~ "Not treated",
      "All treatment types" ~ "Generated"
    )
  ) %>%
  select(
    Year = REF_DATE,
    Units,
    # Geography = GEO,
    Source,
    `Wastewater treatment level`,
    Value = VALUE
  ) %>%
  # Combine the safely treated values together into a single entry
  group_by(Year, Units, Source, `Wastewater treatment level`) %>%
  summarise(Value = sum(Value)) %>%
  # Widen the treatment level column into separate columns for each treatment type for the following operations
  pivot_wider(
    names_from = `Wastewater treatment level`,
    values_from = Value
  ) %>%
  # Treated = Treated + Safely treated
  mutate(
    Treated = Treated + `Safely treated`,
  )

# Calculate proportions of municipal wastewater safely treated/treated/not treated.
municipal_proportions <- municipal_treatment %>%
  mutate(
    Units = "Percentage (%)",
    `Safely treated` = `Safely treated`/Generated*100,
    Treated = Treated/Generated*100,
    `Not treated` = `Not treated`/Generated*100,
  ) %>%
  select(-Generated)

# Tidy data
industrial_treatment <- gather(industrial_treatment, key = "Wastewater treatment level", value = "Value", -Year, -Source, -Units)
industrial_proportions <- gather(industrial_porportions, key = "Wastewater treatment level", value = "Value", -Year, -Source, -Units)
municipal_treatment <- gather(municipal_treatment, key = "Wastewater treatment level", value = "Value", -Year, -Source, -Units)
municipal_proportions <- gather(municipal_proportions, key = "Wastewater treatment level", value = "Value", -Year, -Source, -Units)
  


data_merge <- bind_rows(industrial_proportions,
                        municipal_proportions,
                        industrial_treatment,
                        municipal_treatment) %>%
  mutate(
    Value = round(Value, 2)
  )

# total_line <-
#   data_merge %>% 
#     filter(
#     Geography == "Canada"
#   ) %>%
#   mutate(Geography = "")
# 
# data_with_Canada <- bind_rows(total_line, data_merge)
# 
# data_final <-
#   data_with_Canada %>%
#   filter(
#     !Geography %in% exclude_Canada
#   ) 

write.csv(
  data_merge,
  "data/indicator_6-3-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)  
