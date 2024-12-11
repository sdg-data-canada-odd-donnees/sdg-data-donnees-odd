# Indicator 16.1.4 ---------------------------------------------------------
# Proportion of population that feel safe walking alone around the area they live after dark

library(dplyr)
library(cansim)

raw_data <- get_cansim("43-10-0058-01", factors = FALSE)

safety_perception <- raw_data %>%
  filter(Indicators == "Feeling safe walking in the neighbourhood alone after dark",
         Statistics == "Percentage of persons",
  ) %>%
  select(
    Year = REF_DATE,
    `Visible minority`,
    `Sociodemographic characteristics` = `Selected sociodemographic characteristics`,
    Value = VALUE
  ) %>%
  mutate(
    `Visible minority` = replace(
      `Visible minority`,
      `Visible minority` == "Total – Visible minority",
      "Total population"
      ),
    across(
      c("Visible minority", "Sociodemographic characteristics"),
      ~ replace(., `Visible minority` == "Total population" & `Sociodemographic characteristics` == "Total, 15 years and over", NA)
      )
  )

write.csv(safety_perception, "data/indicator_16-1-4.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")

