library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

financing <- get_cansim("33-10-0432-01", factors = FALSE)

characteristics <- c(
  "All small and medium enterprises, 1 to 499 employees",
  "Employment size, 1 to 4 employees",
  "Employment size, 5 to 19 employees",
  "Employment size, 20 to 99 employees",
  "Employment size, 100 to 499 employees",
  "Atlantic Region",
  "Quebec Region",
  "Ontario Region",
  "Prairies Region",
  "British Columbia and Territories Region",
  "Industries, agriculture, forestry, fishing and hunting (NAICS 11), mining and oil and gas extraction, (NAICS 21) [11]",
  "Industries, construction (NAICS 23) [23]",
  "Industries, manufacturing (NAICS 31-33) [31-33]",
  "Industries, wholesale trade (NAICS 41) [41]",
  "Industries, retail trade (NAICS 44-45) [44-45]",
  "Industries, transportation and warehousing (NAICS 48-49) [48-49]",
  "Industries, professional, scientific and technical services (NAICS 54) [54]",
  "Industries, accommodation and food services (NAICS 72) [72]",
  "Industries, other services (NAICS 81) [81]",
  "Industries, other industries from the North American Industrial Classification System (NAICS)",
  "Industries, special industry aggregations, tourism",
  "Industries, special industry aggregations, information and communication technologies (ICT)",
  "Industries, special industry aggregations, knowledge-based industries (KBI)",
  "Industries, special industry aggregations, advanced technology",
  "Industries, special industry aggregations, advanced manufacturing",
  "Age of primary decision maker, younger than 30 years old",
  "Age of primary decision maker, 30 to 39 years old",
  "Age of primary decision maker, 40 to 49 years old",
  "Age of primary decision maker, 50 to 64 years old",
  "Age of primary decision maker, 65 years old and older",
  "Female ownership, 100%, small and medium enterprises",
  "Demographics, majority ownership, Indigenous",
  "Demographics, majority ownership, visible minority",
  "Demographics, majority ownership, person(s) with a disability"
)

series <- c(
  "Request rate by type of debt financing, term loan",
  "Request rate by type of debt financing, line of credit",
  "Overall approval rate by type of debt financing, line of credit",
  "Overall approval rate by type of debt financing, term loan"
)

data_final <- 
  financing %>% 
  select(
    Year = REF_DATE,
    Series = `Debt financing by instrument type`,
    Char = `Characteristics of small and medium enterprises`,
    Value = VALUE
  ) %>% 
  filter(
    Char %in% characteristics,
    Series %in% series
  )  %>% 
  # distinct(Char) %>%
  mutate(
    id = row_number(),
    Series = str_remove(Series, " by type of debt financing"),
    Char_type = str_extract(Char, "^.*?(?=,)"),
    Char_type = ifelse(is.na(Char_type), "Geography", Char_type),
    Char_type = ifelse(
      Char_type %in% c("Female ownership", "Demographics"),
      "Ownership demographics",
      Char_type
    ), 
    Char = ifelse(Char == "Small and medium enterprises", "Woman", Char),
    Char = ifelse(Char_type == "All small and medium enterprises", "", Char),
    Char = str_remove(Char, "^.*,\\s?"),
    Char = ifelse(
      str_detect(Char, "^[a-z]{1}"),
      str_replace(Char, "^[a-z]{1}", str_to_upper(str_extract(Char, "^[a-z]{1}"))),
      Char
    ),
    Char = str_remove(Char, "\\s?\\[.*\\]")
  ) %>% 
  pivot_wider(
    names_from = Char_type,
    values_from = Char
  ) %>%
  select(-id) %>%
  relocate(Value, .after = last_col())

write.csv(
  data_final,
  "data/indicator_9-3-2.csv",
  row.names = FALSE,
  na = ""
)
