# Indicator 16.1.4 ---------------------------------------------------------
# Proportion of population that feel safe walking alone around the area they live after dark

library(cansim)
library(dplyr)
library(stringr)

community_data <- get_cansim("43-10-0058-01", factors = FALSE)

exclude_vismin <- c(
  "Total – Visible minority"
)

community_filtered <-
  community_data %>%
  filter(
    Indicators == "Feeling safe walking in the neighbourhood alone after dark",
    Statistics == "Percentage of persons"
  ) %>%
  mutate(
    `Racialized groups` = `Visible minority`
  ) %>%
  select(
    Year = REF_DATE,
    `Racialized groups`,
    `Selected sociodemographic characteristics`,
    Value = VALUE
  )

canada_total <-
  community_filtered %>%
  filter(
    `Racialized groups` == "Total – Visible minority"
  ) %>%
  mutate(
    `Racialized groups` = "Total - Canada"
  )

combine <- 
  bind_rows(canada_total,community_filtered) %>%
  filter (
    !`Racialized groups` %in% exclude_vismin
  )

total_line <-
  combine %>%
  filter(
    `Racialized groups` == "Total - Canada"&
      `Selected sociodemographic characteristics` == "Total, 15 years and over"
  ) %>% 
  mutate_at(2:3, ~ "")

disaggregate <-
  combine %>%
  filter(
    !(
    `Racialized groups` == "Total - Canada"&
      `Selected sociodemographic characteristics` == "Total, 15 years and over"
   )
  )

data_final <-
  bind_rows(total_line,disaggregate)

write.csv(
  data_final, 
  "data/indicator_16-1-4.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

