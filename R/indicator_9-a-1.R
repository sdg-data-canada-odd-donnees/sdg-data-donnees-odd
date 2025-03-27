# Indicator 9.a.1 ---------------------------------------------------------
# Total official international support (official development assistance plus other official flows) to infrastructure

options(timeout = 300)
library(dplyr)
library(rjson)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.DPGC.200.14%2B100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&lc=en
# Applied filters: Donor = Canada, Recipient = Developing countries, 
# Sector = Economic infrastructure and services, Measure = Official Development Assistance/Other Official Flows, Flow type = Disbursements, Price base = Constant prices

# Official Development Assistance
url_oda <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.200.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
oda_json <- fromJSON(file = url_oda)
oda_json_data <- t(as.data.frame(oda_json$data$dataSets[[1]]$observations))
oda_json_year <- as.data.frame(oda_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

oda_data <- bind_cols(oda_json_year, oda_json_data) %>%
  select(
    Year = `...1`,
    Measure = `...3`,
    Value = `...2`
  ) %>%
  mutate(
    Measure = "Official Development Assistance"
  )

# Other Official Flows (non Export Credit)
url_oof <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.200.14._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
oof_json <- fromJSON(file = url_oof)
oof_json_data <- t(as.data.frame(oof_json$data$dataSets[[1]]$observations))
oof_json_year <- as.data.frame(oof_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

oof_data <- bind_cols(oof_json_year, oof_json_data) %>%
  select(
    Year = `...1`,
    Measure = `...3`,
    Value = `...2`
  ) %>%
  mutate(
    Measure = "Other Official Flows (non Export Credit)"
  )

# Combine datasets
oda <- bind_rows(oda_data,oof_data) %>%
  mutate_at("Value", as.numeric) %>%
  arrange(Year, Measure)

oda_total <- oda %>%
  summarise(Value = sum(Value), .by = Year)

data_final <- bind_rows(oda, oda_total)

write.csv(data_final, "./data/indicator_9-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
