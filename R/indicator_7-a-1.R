# Indicator 7.a.1 ---------------------------------------------------------
# International financial flows to developing countries in support of 
# clean energy research and development and renewable energy production, 
# including in hybrid systems

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.DPGC.23410%2B23631%2B232%2B23182.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&lc=en
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Energy research/Energy generation, renewable sources/Hybrid energy electric power plants/Electric power transmission and distribution (isolated mini-grids), Measure = Official Development Assistance, Flow type = Disbursements, Price base = Constant prices
url_oda <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.2/CAN.DPGC.23410+23631+232+23182.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

oda_raw <- read_sdmx(url_oda)

oda <- oda_raw %>%
  mutate(
    Sector = case_match(
      SECTOR,
      "23182" ~ "Energy research",
      "232" ~ "Energy generation, renewable sources",
      "23410" ~ "Hybrid energy electric power plants",
      "23631" ~ "Electric power transmission and distribution (isolated mini-grids)"
    )
  ) %>%
  select(
    Year = TIME_PERIOD,
    Sector,
    Value = ObsValue,
  ) %>%
  mutate_at("Value", as.numeric) %>%
  arrange(Year)

oda_total <- oda %>%
  summarise(Value = sum(Value), .by = Year)

write.csv(oda_total, "./data/indicator_7-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
