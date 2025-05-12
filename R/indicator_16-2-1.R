# Indicator 16.2.1 ---------------------------------------------------------
#  Proportion of children aged 1–17 years who experienced any physical punishment and/or psychological aggression by caregivers in the past month

library(cansim)
library(dplyr)
library(stringr)

childhood_data <- get_cansim("35-10-0167-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

age <- c(
  "Total, age",
  "15 to 24 years",
  "25 to 34 years",
  "35 to 44 years",
  "45 to 54 years",
  "55 years and older" 
)

immigration_status <- c(
  "Total, immigrant status",
  "Immigrants",
  "Not stated, immigrant status"
)

visible_minority <- c(
  "Total, visible minority population",
  "Visible minority population",
  "Not a visible minority",
  "Not stated, visible minority population"
)

indigenous_identity <- c(
  "Total, population by indigenous identity",
  "Indigenous identity",
  "First Nations (North American Indian)",
  "Métis",
  "Inuk (Inuit)",
  "Other Indigenous identity",
  "Non-indigenous identity",
  "Not stated, population by indigenous identity"
)

disability <- c(
  "Total, disability status",
  "Has a disability",
  "Does not have a disability"
)

sexual_orientation <- c(
  "Total, sexual orientation",
  "Heterosexual",
  "Sexual minority",
  "Lesbian or gay",
  "Bisexual or pansexual",
  "Sexual orientation not elsewhere classified",
  "Not stated, sexual orientation"
)

childhood_filter <-
  childhood_data %>%
  filter(
    UOM == "Percent",
    Statistics == "Percentage"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of childhood maltreatment`,
    `Selected demographic characteristics`,
    Gender,
    Value = VALUE    
  )

childhood_disag_age <-
  childhood_filter %>%
  filter(
    `Selected demographic characteristics` %in% age
  ) %>%
  mutate(
    `Age` = `Selected demographic characteristics`
  ) %>%
  select(
    Year,
    Geography,
    `Type of childhood maltreatment`,
    `Age`,
    Gender,
    Value  
  ) 

childhood_disag_immig <-
  childhood_filter %>%
  filter(
    `Selected demographic characteristics` %in% immigration_status
  ) %>%
  mutate(
    `Immigrant status` = `Selected demographic characteristics`
  ) %>%
  select(
    Year,
    Geography,
    `Type of childhood maltreatment`,
    `Immigrant status`,
    Gender,
    Value  
  ) 

childhood_disag_vismin <-
  childhood_filter %>%
  filter(
    `Selected demographic characteristics` %in% visible_minority
  ) %>%
  mutate(
    `Visible minority` = `Selected demographic characteristics`
  ) %>%
  select(
    Year,
    Geography,
    `Type of childhood maltreatment`,
    `Visible minority`,
    Gender,
    Value  
  ) 

childhood_disag_ind <-
  childhood_filter %>%
  filter(
    `Selected demographic characteristics` %in% indigenous_identity
  ) %>%
  mutate(
    `Indigenous identity` = `Selected demographic characteristics`
  ) %>%
  select(
    Year,
    Geography,
    `Type of childhood maltreatment`,
    `Indigenous identity`,
    Gender,
    Value  
  ) 
  
childhood_disag_dis <-
  childhood_filter %>%
  filter(
    `Selected demographic characteristics` %in% disability
  ) %>%
  mutate(
    `Disability` = `Selected demographic characteristics`
  ) %>%
  select(
    Year,
    Geography,
    `Type of childhood maltreatment`,
    `Disability`,
    Gender,
    Value  
  ) 

childhood_disag_sexual_or <-
  childhood_filter %>%
  filter(
    `Selected demographic characteristics` %in% sexual_orientation
  ) %>%
  mutate(
    `Sexual orientation` = `Selected demographic characteristics`
  ) %>%
  select(
    Year,
    Geography,
    `Type of childhood maltreatment`,
    `Sexual orientation`,
    Gender,
    Value  
  ) 

bind_all <- 
  bind_rows(childhood_disag_age,childhood_disag_immig,childhood_disag_vismin,childhood_disag_ind,childhood_disag_dis,childhood_disag_sexual_or) %>%
  relocate(Geography, .after = `Type of childhood maltreatment`) %>%
  relocate(Age, .after = Gender) %>%
  relocate(`Immigrant status`, .after = Age) %>%
  relocate(`Visible minority`, .after = `Immigrant status`) %>%
  relocate(`Indigenous identity`, .after = `Visible minority`) %>%
  relocate(`Disability`, .after = `Indigenous identity`) %>%
  relocate(`Sexual orientation`, .after = `Disability`) %>%
  filter(
    !is.na(Value)
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    # total line
    bind_all %>%
      filter(
        Geography == "Canada"&
        `Type of childhood maltreatment` == "Physical or sexual abuse"&
        Age == "Total, age" &
        Gender == "Total, gender"
      ) %>% 
      mutate_at(2:6, ~ ""),
    
    # disaggregates
    bind_all %>%
      filter(
        !(
          Geography == "Canada"&
          `Type of childhood maltreatment` == "Physical or sexual abuse"&
          Age == "Total, age" &
          Gender == "Total, gender"
        )
      )
  )

write.csv(
  data_final, 
  "data/indicator_16-2-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
