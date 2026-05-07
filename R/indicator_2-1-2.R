# GIF 2.1.2 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

# load CODR table from stc api
economic_families <- get_cansim("13-10-0834-01", factors = FALSE)
demographic_characteristics <- get_cansim("13-10-0835-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

age <- c(
  "under 18 years",
  "18 to 64 years",
  "18 to 24 years",
  "25 to 34 years",
  "35 to 44 years",
  "45 to 54 years",
  "55 to 64 years",
  "65 years and over"
)

woman <- c(
  "Persons in one-parent families where the parent is a woman+",
  "Non-senior women+ not in an economic family",
  "Senior women+ not in an economic family"
) %>%
  append(paste("Women+", age, sep = ", "))

man <- c(
  "Persons in one-parent families where the parent is a man+",
  "Non-senior men+ not in an economic family",
  "Senior men+ not in an economic family"
) %>%
  append(paste("Men+", age, sep = ", "))

persons <- paste("Persons", age, sep = " ")

vismin <- c(
  "Visible minority population",
  "South Asian",
  "Chinese",
  "Black",
  "Filipino",
  "Arab",
  "Latin American",
  "Southeast Asian",
  "Other visible minority",
  "Not a visible minority"
)

indigenous <- c(
  "Indigenous population",
  "Non-Indigenous population",
  "Not a visible minority nor Indigenous"
)

pop_aged_15_plus <- c(
  "All persons aged 15 years and over",
  "Indigenous population aged 15 years and over",
  "First Nations aged 15 years and over",
  "Métis aged 15 years and over",
  "Non-Indigenous population aged 15 years and over",
  "Immigrants aged 15 years and over",
  "Recent immigrants (10 years or less) aged 15 years and over",
  "Very recent immigrants (5 years or less) aged 15 years and over",
  "Persons aged 15 years and over born in Canada"
)

# Manual input data for territories
territories <- c("Yukon", "Northwest Territories", "Nunavut")
nterritories <- length(territories)
years <- c("2020", "2021", "2022")
nyears <- length(years)

values_insecure <- c(
  21.2, 20.4, 49.5,
  12.8, 22.2, 46.1,
  21.4, 27.6, 62.6
)

values_secure <- 100 - values_insecure

df_territories <- tibble(
  Year = rep(years, each = nterritories),
  Geography = rep(territories, nyears),
  `Household food security status` = "Food insecure",
  Value = values_insecure,
) %>%
  add_row(
    Year = rep(years, each = nterritories),
    Geography = rep(territories, nyears),
    `Household food security status` = "Food secure",
    Value = values_secure,
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

# -------------------------------------------------------------------------

filter_economic_families <-
  economic_families %>%
  filter(Statistics == "Percentage of persons") %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Economic family type`,
    `Household food security status`,
    Value = VALUE
  ) %>%
  na.omit() %>%
  mutate(
    Gender = case_when(
      str_detect(`Economic family type`, "woman\\+|women\\+") ~ "Woman+",
      str_detect(`Economic family type`, "man\\+|men\\+") ~ "Man+",
      TRUE ~ "All genders"
    ),
    `Economic family type` = str_remove_all(`Economic family type`, "woman\\+\\b"),
    `Economic family type` = str_remove_all(`Economic family type`, "women\\+\\b"),
    `Economic family type` = str_remove_all(`Economic family type`, "man\\+\\b"),
    `Economic family type` = str_remove_all(`Economic family type`, "men\\+\\b"),
    `Economic family type` = str_replace_all(`Economic family type`, "Senior not ", "Seniors not "),
    `Economic family type` = str_replace_all(`Economic family type`, "Non-senior not ", "Non-seniors not "),
    `Economic family type` = str_remove_all(`Economic family type`, " where the parent is a woman\\+\\b"),
    `Economic family type` = str_remove_all(`Economic family type`, " where the parent is a man\\+\\b")
  ) %>%
  relocate(Gender, .before = `Household food security status`)

# -------------------------------------------------------------------------

filter_demographic_characteristics <-
  demographic_characteristics %>%
  filter(Statistics == "Percentage of persons") %>%
  filter(`Demographic characteristics` != "All persons") %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Demographic characteristics`,
    `Household food security status`,
    Value = VALUE
  ) %>%
  na.omit() %>%
  mutate(
    Gender = case_when(
      str_detect(`Demographic characteristics`, "woman\\+|women\\+") ~ "Woman+",
      str_detect(`Demographic characteristics`, "man\\+|men\\+") ~ "Man+",
      `Demographic characteristics` %in% persons ~ "All genders",
      `Demographic characteristics` == "Women+" ~ "Woman+",
      `Demographic characteristics` == "Men+" ~ "Man+"
    ),
    `Age group` = case_when(
      str_detect(`Demographic characteristics`, "Women\\+") ~ str_to_sentence(str_remove_all(`Demographic characteristics`, "Women\\+, ")),
      str_detect(`Demographic characteristics`, "Men\\+") ~ str_to_sentence(str_remove_all(`Demographic characteristics`, "Men\\+, ")),
      `Demographic characteristics` %in% persons ~ str_to_sentence(str_remove_all(`Demographic characteristics`, "Persons "))
    ),
    `Economic family type` = case_when(
      `Demographic characteristics` == "Women+" ~ "All persons",
      `Demographic characteristics` == "Men+" ~ "All persons"
    ),
    `Visible minority` = case_when(
      `Demographic characteristics` %in% vismin ~ `Demographic characteristics`
    ),
    `Indigenous population` = case_when(
      `Demographic characteristics` %in% indigenous ~ `Demographic characteristics`
    ),
    `Population aged 15 years and over` = case_when(
      `Demographic characteristics` %in% pop_aged_15_plus ~ str_remove_all(`Demographic characteristics`, " aged 15 years and over")
    )
  )

# -------------------------------------------------------------------------

food_insecurity <- bind_rows(
  filter_economic_families,
  filter_demographic_characteristics
) %>%
  filter(
    !`Demographic characteristics` %in% pop_aged_15_plus
  ) %>%
  select(
    Year,
    `Household food security status`,
    Geography,
    Gender,
    `Age group`,
    `Economic family type`,
    `Visible minority`,
    `Indigenous population`,
    Value
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) %>%
  mutate(
    across(
      c(Geography, Gender, `Economic family type`, `Household food security status`),
      ~ replace(., Geography == "Canada" &
        Gender == "All genders" &
        `Economic family type` == "All persons" &
        `Household food security status` == "Food insecure, moderate or severe",
        NA)
    )
  )

data_final <- bind_rows(food_insecurity, df_territories)

# Write the csv file
write.csv(
  data_final,
  "data/indicator_2-1-2.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)