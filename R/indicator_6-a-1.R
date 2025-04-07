# Indicator 6.a.1 ---------------------------------------------------------
# Amount of water- and sanitation-related official development 
# assistance that is part of a government-coordinated spending plan

options(timeout = 300)
library(dplyr)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&pg=0&fc=Topic&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.DPGC.140%2B31140.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Water supply & sanitation/Agricultural water resources, Measure = Official Development Assistance, Flow type = Disbursements, Price base = Constant prices
url_oda <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.140+31140.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfile"

oda_raw <- read.csv(url_oda)

oda <- oda_raw %>%
  mutate(
    Sector = case_when(
      SECTOR == "140" ~ "Water supply and sanitation",
      SECTOR == "31140" ~ "Agricultural water resources",
    )
  ) %>%
  select(
    Year = TIME_PERIOD,
    Sector,
    Value = OBS_VALUE,
  ) %>%
  mutate_at("Value", as.numeric) %>%
  arrange(Year)

oda_total <- oda %>%
  summarise(Value = sum(Value), .by = Year)

data_final <- bind_rows(oda, oda_total)

write.csv(data_final, "./data/indicator_6-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
