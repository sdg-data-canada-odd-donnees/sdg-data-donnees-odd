# Indicator 9.a.1 ---------------------------------------------------------
# Total official international support (official development assistance plus other official flows) to infrastructure

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.DPGC.200.14%2B100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&lc=en
# Applied filters: Donor = Canada, Recipient = Developing countries, 
# Sector = Economic infrastructure and services, Measure = Official Development Assistance/Other Official Flows, Flow type = Disbursements, Price base = Constant prices
url_oda <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.2/CAN.DPGC.200.14+100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

oda_raw <- read_sdmx(url_oda)

oda <- oda_raw %>%
  mutate(
    MEASURE = case_match(
      MEASURE,
      "14" ~ "Other Official Flows (non Export Credit)",
      "100" ~ "Official Development Assistance"
    ),
  ) %>%
  select(
    Year = TIME_PERIOD,
    Measure = MEASURE,
    Value = ObsValue,
  ) %>%
  mutate_at("Value", as.numeric) %>%
  arrange(Year, Measure)

oda_total <- oda %>%
  summarise(Value = sum(Value), .by = Year)

data_final <- bind_rows(oda, oda_total)

write.csv(data_final, "./data/indicator_9-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
