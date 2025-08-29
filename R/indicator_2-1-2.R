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

age <-c(
  "under 18 years",
  "18 to 64 years",
  "18 to 24 years",
  "25 to 34 years",
  "35 to 44 years",
  "45 to 54 years",
  "55 to 64 years",
  "65 years and over"
)

female <- c(
  "Persons in female lone-parent families",
  "Elderly females not in an economic family",
  "Non-elderly females not in an economic family",
  "Non-senior females not in an economic family",
  "Senior females not in an economic family"
) %>% 
  append(paste("Females", age, sep = ", "))

male <- c(
  "Persons in male lone-parent families",
  "Elderly males not in an economic family",
  "Non-elderly males not in an economic family",
  "Non-senior males not in an economic family",
  "Senior males not in an economic family"
) %>% 
  append(paste("Males", age, sep = ", "))

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
# Sources
# 2020: https://www150.statcan.gc.ca/n1/en/daily-quotidien/221103/dq221103d-eng.pdf
# 2021: https://www150.statcan.gc.ca/n1/daily-quotidien/230621/dq230621c-eng.htm
# 2022: https://www150.statcan.gc.ca/n1/daily-quotidien/240619/dq240619d-eng.htm
territories <- c("Yukon", "Northwest Territories", "Nunavut")
nterritories <- length(territories)
years <- c("2020", "2021", "2022")
nyears <- length(years)
# Sources give % of food insecure households (marginal, moderate or severe)
values_insecure <- c(21.2, # 2020 YT
                     20.4, #      NT
                     49.5, #      NU
                     12.8, # 2021 YT
                     22.2, #      NT
                     46.1, #      NU
                     21.4, # 2022 YT
                     27.6, #      NT
                     62.6) #      NU
# % of food secure = 100 - % of food insecure
values_secure <- 100 - values_insecure

# Build data frame for manually input data for territories
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


# Format the table 
filter_economic_families <- 
  economic_families %>%
  filter(Statistics == "Percentage of persons") %>%
  select(Year = REF_DATE, 
         Geography = GEO, 
         `Economic family type`, 
         `Household food security status`, 
         Value = VALUE) %>%
  na.omit() %>%
  mutate(
    Sex = case_when(
      `Economic family type` %in% female ~ "Female",
      `Economic family type` %in% male ~ "Male",
      TRUE ~ "Both sexes"
    ),
    `Economic family type` = str_remove_all(`Economic family type`,"female " ),
    `Economic family type` = str_remove_all(`Economic family type`,"females " ),
    `Economic family type` = str_remove_all(`Economic family type`,"male " ),
    `Economic family type` = str_remove_all(`Economic family type`,"males " ),
    `Economic family type` = str_replace_all(`Economic family type`,"Elderly not ", "Elderly persons not " ),
    `Economic family type` = str_replace_all(`Economic family type`,"Non-elderly not ", "Non-elderly persons not " ),
    `Economic family type` = str_replace_all(`Economic family type`,"Senior not ", "Seniors not " ),
    `Economic family type` = str_replace_all(`Economic family type`,"Non-senior not ", "Non-seniors not " ),
  ) %>%
  relocate(Sex, .before = `Household food security status`)

filter_demographic_characteristics <-
  demographic_characteristics %>%
  filter(Statistics == "Percentage of persons") %>%
  filter(`Demographic characteristics` != "All persons") %>%
  select(Year = REF_DATE, 
         Geography = GEO, 
         `Demographic characteristics`, 
         `Household food security status`, 
         Value = VALUE) %>%
  na.omit() %>%
  mutate(
    Sex = case_when(
      `Demographic characteristics` %in% female ~ "Female",
      `Demographic characteristics` %in% male ~ "Male",
      `Demographic characteristics` %in% persons ~ "Both sexes",
      `Demographic characteristics` == "Females" ~ "Female",
      `Demographic characteristics` == "Males" ~ "Male"
    ),
    `Age group` = case_when(
      `Demographic characteristics` %in% female ~ str_to_sentence(str_remove_all(`Demographic characteristics`,"Females, ")),
      `Demographic characteristics` %in% male ~ str_to_sentence(str_remove_all(`Demographic characteristics`,"Males, ")),
      `Demographic characteristics` %in% persons ~ str_to_sentence(str_remove_all(`Demographic characteristics`,"Persons "))
    ),
    `Economic family type` = case_when(
      `Demographic characteristics` == "Females" ~ "All persons",
      `Demographic characteristics` == "Males" ~ "All persons"
    ),
    `Visible minority` = case_when(
      `Demographic characteristics` %in% vismin ~ `Demographic characteristics`
    ),
    `Indigenous population` = case_when(
      `Demographic characteristics` %in% indigenous ~ `Demographic characteristics`
    ),
    `Population aged 15 years and over` = case_when(
      `Demographic characteristics` %in% pop_aged_15_plus ~ str_remove_all(`Demographic characteristics`," aged 15 years and over")
    )
  )

food_insecurity <- bind_rows(filter_economic_families,
                             filter_demographic_characteristics) %>%
  filter(
    !`Demographic characteristics` %in% pop_aged_15_plus
  ) %>%
  select(
    Year,
    `Household food security status`,
    Geography,
    `Sex`,
    `Age group`,
    `Economic family type`,
    `Visible minority`,
    `Indigenous population`,
    # `Population aged 15 years and over`,
    Value
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

# Create the aggregate line
total_line <- 
  food_insecurity %>%
  filter(Geography == "Canada", `Economic family type` == "All persons", Sex == "Both sexes",
         `Household food security status` == "Food insecure, moderate or severe") %>%
  mutate_at(2:(ncol(.)-2), ~ NA)

# Create the non - aggregate data 
non_total <-
  food_insecurity %>%
  filter(!(Geography == "Canada" & `Economic family type` == "All persons" & Sex == "Both sexes" & 
             `Household food security status` == "Food insecure, moderate or severe"))

# Add the aggregate and non - aggregate data
data_final <- bind_rows(total_line, 
                        df_territories, 
                        non_total)

# Write the csv file
write.csv(data_final, "data/indicator_2-1-2.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")