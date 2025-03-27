# Indicator 1.a.1 ---------------------------------------------------------
# 1.a.1 Total official development assistance grants from all donors that
#       focus on poverty reduction as a share of the recipient country’s 
#       gross national income

options(timeout = 300)
library(dplyr)
library(rjson)
library(readsdmx)

# API URLs for ODA grants and GNI

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?df[ds]=DisseminateFinalBoost&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&dq=CAN.DPGC.112%2B122%2B140%2B520%2B16050.11._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Basic education/Basic health/Water supply & sanitation/Multisector aid for basic social services/Development food assistance, Measure = ODA Grants, Price base = Constant prices

# Basic education
url_education <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.112.11._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
education_json <- fromJSON(file = url_education)
education_json_data <- t(as.data.frame(education_json$data$dataSets[[1]]$observations))
education_json_year <- as.data.frame(education_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

education_data <- bind_cols(education_json_year, education_json_data) %>%
  select(
    Year = `...1`,
    Sector = `...3`,
    ODA = `...2`
  ) %>%
  mutate(
    Sector = "Basic education"
  )

# Basic health
url_health <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.122.11._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
health_json <- fromJSON(file = url_health)
health_json_data <- t(as.data.frame(health_json$data$dataSets[[1]]$observations))
health_json_year <- as.data.frame(health_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

health_data <- bind_cols(health_json_year, health_json_data) %>%
  select(
    Year = `...1`,
    Sector = `...3`,
    ODA = `...2`
  ) %>%
  mutate(
    Sector = "Basic health"
  )

# Water supply and sanitation
url_water <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.140.11._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
water_json <- fromJSON(file = url_water)
water_json_data <- t(as.data.frame(water_json$data$dataSets[[1]]$observations))
water_json_year <- as.data.frame(water_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

water_data <- bind_cols(water_json_year, water_json_data) %>%
  select(
    Year = `...1`,
    Sector = `...3`,
    ODA = `...2`
  ) %>%
  mutate(
    Sector = "Water supply and sanitation"
  )

# Multisector aid for basic social services
url_social <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.16050.11._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
social_json <- fromJSON(file = url_social)
social_json_data <- t(as.data.frame(social_json$data$dataSets[[1]]$observations))
social_json_year <- as.data.frame(social_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

social_data <- bind_cols(social_json_year, social_json_data) %>%
  select(
    Year = `...1`,
    Sector = `...3`,
    ODA = `...2`
  ) %>%
  mutate(
    Sector = "Multisector aid for basic social services"
  )

# Development food assistance
url_food <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.520.11._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
food_json <- fromJSON(file = url_food)
food_json_data <- t(as.data.frame(food_json$data$dataSets[[1]]$observations))
food_json_year <- as.data.frame(food_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

food_data <- bind_cols(food_json_year, food_json_data) %>%
  select(
    Year = `...1`,
    Sector = `...3`,
    ODA = `...2`
  ) %>%
  mutate(
    Sector = "Development food assistance"
  )


# URL of source for GNI
# DAC1: Flows by donor (ODA+OOF+Private). See https://data-explorer.oecd.org/vis?lc=en&pg=0&snb=1&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_DAC1%40DF_DAC1&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.1....Q.&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb
# Applied filters: Donor = Canada, Measure = Gross National Income (GNI), Price base = Constant prices
url_gni <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.2/CAN.1....Q.?startPeriod=2015&dimensionAtObservation=AllDimensions" # source used by UK

gni_raw <- read_sdmx(url_gni)

# Stop and throw error if any dollar values use different base years
#if (any(sort(unique(oda_raw$BASE_PER)) != sort(unique(gni_raw$BASE_PER)))) {
#  print("ERROR: Dollar amounts from different base years cannot be combined.")
#  stop()
#}

gni <- gni_raw %>%
  select(
    Year = TIME_PERIOD,
    GNI = ObsValue
  ) %>%
  mutate_at("GNI", as.numeric)

oda <- bind_rows(education_data,health_data,water_data,social_data,food_data) %>%
  mutate_at("ODA", as.numeric) %>%
  arrange(Year)

oda_total <- oda %>%
  summarise(ODA = sum(ODA), .by = c(Year))

data_final <- bind_rows(oda, oda_total) %>%
  # Calculate ODA as percentage of GNI
  left_join(gni, by = "Year") %>%
  mutate(Value = ODA / GNI * 100) %>%
  select(
    Year,
    `Poverty reduction aid type` = Sector,
    Value
  )

write.csv(data_final, "./data/indicator_1-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
