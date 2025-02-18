# Indicator 16.b.1 ---------------------------------------------------------
# Proportion of population reporting having personally felt discriminated against or harassed in the previous 12 months on the basis of a ground of discrimination prohibited under international human rights law

# load libraries
library(cansim)
library(dplyr)
library(stringr)

discrimination_raw <- get_cansim("45-10-0100-01", factors = FALSE)
discrimination_disag_raw <- get_cansim("45-10-0101-01", factors = FALSE)

age <- c(
  "Total, 15 years and over",
  "15 to 24 years",
  "25 to 54 years",
  "25 to 34 years",
  "35 to 44 years",
  "45 to 54 years",
  "55 to 64 years",
  "65 years and over",
  "65 to 74 years",
  "75 years and over"
)

immigrant <- c(
  "Total, by immigrant status",
  "Non-immigrants",
  "Immigrants",
  "Immigrants, 10 years or less in Canada",
  "Immigrants, more than 10 years in Canada",
  "Immigrants, years in Canada not stated",
  "Non-permanent residents"
)

vis_min <- c(
  "Total, by visible minority group",
  "Visible minority population",
  "South Asian",
  "Chinese",
  "Black",
  "Filipino",
  "Arab",
  "Latin American",
  "Southeast Asian",
  "West Asian",
  "Korean",
  "Japanese",
  "Visible minority, not included elsewhere",
  "Multiple visible minorities",
  "Not a visible minority"
)

indigenous <- c(
  "Total, by Indigenous identity",
  "Indigenous identity",
  "Single Indigenous identity",
  "First Nations",
  "Métis",
  "Inuk (Inuit)",
  "Multiple Indigenous identities",
  "Non-Indigenous identity"
)

disability <- c(
  "Total, persons with and without a disability, difficulty or long-term condition",
  "Persons with a disability, difficulty or long-term condition",
  "Persons without a disability, difficulty or long-term condition"
)

lgbtq2 <- c(
  "Total, LGBTQ2+ and non-LGBTQ2+ people",
  "LGBTQ2+ people",
  "Non-LGBTQ2+ people"
)

education <- c(
  "Total, highest certificate, diploma or degree",
  "No certificate, diploma or degree",
  "Secondary (high) school diploma or equivalency certificate",
  "Postsecondary certificate, diploma or degree",
  "Postsecondary certificate or diploma below bachelor level",
  "Apprenticeship or trades certificate or diploma",
  "College, CEGEP or other non-university certificate or diploma",
  "University certificate or diploma below bachelor level",
  "Bachelor's degree or higher",
  "Bachelor's degree",
  "University certificate, diploma or degree above bachelor level"
)

activity <- c(
  "Total, by main activity",
  "Working at a paid job or business",
  "Retired",
  "Other activity"
)

location <- c(
  "Total, urban and rural areas",
  "Urban areas",
  "Rural areas"
)

total_disags <- c(
  "Total, 15 years and over",
  "Total, by immigrant status",
  "Total, by visible minority group",
  "Total, by Indigenous identity",
  "Total, persons with and without a disability, difficulty or long-term condition",
  "Total, LGBTQ2+ and non-LGBTQ2+ people",
  "Total, highest certificate, diploma or degree",
  "Total, by main activity",
  "Total, urban and rural areas"
)



# Load geocodes from a CSV file
geocodes <- read.csv("geocodes.csv")

discrimination <- 
  discrimination_raw %>%
  filter(
    Statistics == "Percentage of persons",
    !GEO %in% c("Atlantic Region", "Prairies Region"),
  ) %>%
  mutate(
    REF_DATE = str_replace_all(REF_DATE, "-01", " Q1"),
    REF_DATE = str_replace_all(REF_DATE, "-04", " Q2"),
    REF_DATE = str_replace_all(REF_DATE, "-07", " Q3"),
    REF_DATE = str_replace_all(REF_DATE, "-10", " Q4"),
    Gender = str_replace_all(Gender, "Total, all persons", "Total")
  ) %>%
  select(
    Year = REF_DATE,
    `Discrimination indicators` = Indicators,
    Geography = GEO,
    Gender,
    Value = VALUE
  ) %>%
  mutate(
    Geography = str_remove(Geography, " \\(.*\\)")
  ) %>%
  na.omit()

discrimination_disag <-
  discrimination_disag_raw %>%
  filter(
    Statistics == "Percentage of persons",
    !GEO %in% c("Atlantic Region", "Prairies Region"),
  ) %>%
  mutate(
    REF_DATE = str_replace_all(REF_DATE, "-01", " Q1"),
    REF_DATE = str_replace_all(REF_DATE, "-04", " Q2"),
    REF_DATE = str_replace_all(REF_DATE, "-07", " Q3"),
    REF_DATE = str_replace_all(REF_DATE, "-10", " Q4"),
    Gender = str_replace_all(Gender, "Total, all persons", "Total")
  ) %>%
  select(
    Year = REF_DATE,
    `Discrimination indicators` = Indicators,
    Geography = GEO,
    Gender,
    `Sociodemographic characteristics`,
    Value = VALUE
  ) %>%
  mutate(
    Geography = str_remove(Geography, " \\(.*\\)")
  ) %>%
  na.omit() %>%
  mutate(
    `Age group` = case_when(
      `Sociodemographic characteristics` %in% age ~ `Sociodemographic characteristics`
    ),
    `Immigrant status` = case_when(
      `Sociodemographic characteristics` %in% immigrant ~ `Sociodemographic characteristics`
    ),
    `Visible minority` = case_when(
      `Sociodemographic characteristics` %in% vis_min ~ `Sociodemographic characteristics`
    ),
    `Indigenous identity` = case_when(
      `Sociodemographic characteristics` %in% indigenous ~ `Sociodemographic characteristics`
    ),
    `Disability` = case_when(
      `Sociodemographic characteristics` %in% disability ~ `Sociodemographic characteristics`
    ),
    `LGBTQ2+` = case_when(
      `Sociodemographic characteristics` %in% lgbtq2 ~ `Sociodemographic characteristics`
    ),
    `Highest level of education` = case_when(
      `Sociodemographic characteristics` %in% education ~ `Sociodemographic characteristics`
    ),
    `Main activity` = case_when(
      `Sociodemographic characteristics` %in% activity ~ `Sociodemographic characteristics`
    ),
    `Location` = case_when(
      `Sociodemographic characteristics` %in% location ~ `Sociodemographic characteristics`
    )
  ) %>%
  relocate(Value, .after = "Location") %>%
  select(
    -`Sociodemographic characteristics`
  )

combined <-
  bind_rows(discrimination_disag,discrimination)
  
main_series_total_line <-
  combined %>%
  filter(
    `Discrimination indicators` == "Experienced discrimination or unfair treatment in Canada",
    Geography == "Canada",
    Gender == "Total",
    `Age group` %in% total_disags |
    `Immigrant status` %in% total_disags |
    `Visible minority` %in% total_disags |
    `Indigenous identity` %in% total_disags |
    `Disability` %in% total_disags |
    `LGBTQ2+` %in% total_disags |
    `Highest level of education` %in% total_disags |
    `Main activity` %in% total_disags |
    `Location` %in% total_disags
  ) %>% 
  mutate_at(2:(ncol(.) - 1), ~ "") %>%
  distinct() %>%
  mutate(
    Series = "Proportion of population reporting to have experienced discrimination or unfair treatment"
  ) %>%
  relocate(Series, .before = "Geography")

main_series_non_total <-
  combined %>%
  filter(
    !(`Discrimination indicators` == "Experienced discrimination or unfair treatment in Canada" &
    Geography == "Canada" &
    Gender == "Total" &
    (`Age group` %in% total_disags |
      `Immigrant status` %in% total_disags |
      `Visible minority` %in% total_disags |
      `Indigenous identity` %in% total_disags |
      `Disability` %in% total_disags |
      `LGBTQ2+` %in% total_disags |
      `Highest level of education` %in% total_disags |
      `Main activity` %in% total_disags |
      `Location` %in% total_disags)
    )
  ) %>%
  filter(
    `Discrimination indicators` == "Experienced discrimination or unfair treatment in Canada" |
      `Discrimination indicators` == "Did not experience discrimination or unfair treatment in Canada"
  ) %>%
  mutate(
    Series = "Proportion of population reporting to have experienced discrimination or unfair treatment"
  ) %>%
  relocate(Series, .before = "Geography")

second_series_total_line <-
  combined %>%
  filter(
    Geography == "Canada",
        Gender == "Total",
        `Age group` == "Total, 15 years and over"
   ) %>%
  filter(
    !(`Discrimination indicators` == "Experienced discrimination or unfair treatment in Canada" |
        `Discrimination indicators` == "Did not experience discrimination or unfair treatment in Canada")
  ) %>% 
  mutate_at(3:(ncol(.) - 1), ~ "") %>%
  mutate(
    Series = "Proportion of individuals within a population who report experiencing discrimination or unfair treatment"
  ) %>%
  relocate(Series, .before = "Geography")

second_series_non_total <-
  combined %>%
  filter(
    !(Geography == "Canada"&
      Gender == "Total" &
      (`Age group` %in% total_disags |
         `Immigrant status` %in% total_disags |
         `Visible minority` %in% total_disags |
         `Indigenous identity` %in% total_disags |
         `Disability` %in% total_disags |
         `LGBTQ2+` %in% total_disags |
         `Highest level of education` %in% total_disags |
         `Main activity` %in% total_disags |
         `Location` %in% total_disags)
    )
  ) %>%
  filter(
    !(`Discrimination indicators` == "Experienced discrimination or unfair treatment in Canada" |
      `Discrimination indicators` == "Did not experience discrimination or unfair treatment in Canada")
  ) %>%
  mutate(
    Series = "Proportion of individuals within a population who report experiencing discrimination or unfair treatment"
  ) %>%
  relocate(Series, .before = "Geography")

data_final <-
  bind_rows(main_series_total_line,main_series_non_total,second_series_total_line,second_series_non_total) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

write.csv(
  data_final,
  "data/indicator_16-b-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)