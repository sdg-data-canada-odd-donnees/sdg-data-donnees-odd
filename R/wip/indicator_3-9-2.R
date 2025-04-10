#
# # 3.9.2 -------------------------------------------------------------------

# TODO: rewrite so that get_cansim line only contains

library(dplyr)
library(cansim)
library(stringr)
library(purrr)

chapter_tbls <- c(
  "13-10-0141-01",
  "13-10-0144-01",
  "13-10-0780-01",
  "13-10-0782-01",
  "13-10-0391-01",
  "13-10-0153-01"
)

causes_of_death <- tribble(
  ~cause, ~`Type of disease`,
  "Typhoid and paratyphoid fevers", "diarrhea",
  "Shigellosis", "diarrhea",
  "Other bacterial intestinal infections", "diarrhea",
  "Amoebiasis", "diarrhea",
  "Other protozoal intestinal diseases", "diarrhea",
  "Viral and other specified intestinal infections", "diarrhea",
  "Other gastroenteritis and colitis of infectious and unspecified origin", "diarrhea",
  "Ascariasis", "intestinal nematode infections",
  "Malnutrition", "protein-energy malnutrition",
  "Nonsuppurative otitis media", "acute respiratory infections",
  "Suppurative and unspecified otitis media", "acute respiratory infections",
  "Acute upper respiratory infections", "acute respiratory infections",
  "Influenza and pneumonia", "acute respiratory infections",
  "Other acute lower respiratory infections", "acute respiratory infections",
  "Severe acute respiratory syndrome", "acute respiratory infections",
  "Congenital pneumonia", "acute respiratory infections"
) %>%
  mutate(`Type of disease` = str_to_sentence(`Type of disease`))

get_cause_of_death_data <- function(tbl_no) {

  get_cansim(tbl_no, factors = FALSE) %>%
  mutate(
    `Cause of death (ICD-10)` = str_remove(`Cause of death (ICD-10)`, " \\[.*\\]")
  ) %>%
  filter(
    REF_DATE >= 2015,
    `Age group` == "Total, all ages",
    Sex == "Both sexes",
    `Cause of death (ICD-10)` %in% causes_of_death$cause
  ) %>%
  select(
    Year = REF_DATE,
    `Cause of death (ICD-10)`,
    Value = VALUE
  )

}

pop <-
  get_cansim("17-10-0005-01", factors = FALSE) %>%
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>%
  select(
    Year = REF_DATE,
    Population = VALUE
  )

data <- purrr::map_dfr(chapter_tbls, get_cause_of_death_data)

data <-
  data %>%
  left_join(causes_of_death, by = c("Cause of death (ICD-10)" = "cause")) %>%
  group_by(Year, `Type of disease`) %>%
  summarise(Deaths = sum(Value, na.rm = TRUE), .groups = "keep") %>%
  left_join(pop) %>%
  transmute(Value = round((Deaths/Population)*100000, 3))

write.csv(
  data,
  "data/indicator_3-9-2.csv",
  na = "",
  row.names = FALSE
)
