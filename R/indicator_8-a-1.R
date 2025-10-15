# Indicator 8.a.1 ---------------------------------------------------------
# Aid for Trade commitments and disbursements

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.DPGC.332%2B230%2B210%2B220%2B240%2B250%2B311%2B312%2B313%2B321%2B322%2B331.100._T._T.C%2BD.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&lc=en
# Donor = Canada
# Recipient = Developing countries,
# Sector = Trade policies and regulations + Transport and storage + Communications +
#           Energy + Banking and financial services + Business and other services +
#           Agriculture + Forestry + Fishing + Industry +
#           Mineral resources and mining + Tourism
# Measure = Official Development Assistance
# Flow type = Disbursements/Commitments, Price base = Constant prices
url_oda <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.332+230+210+220+240+250+311+312+313+321+322+331.100._T._T.C+D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

oda_raw <- read_sdmx(url_oda)

oda <- oda_raw %>%
  mutate(
    `Flow type` = case_match(
      FLOW_TYPE,
      "C" ~ "Commitments",
      "D" ~ "Disbursements"
    ),
    Sector = case_when(
      SECTOR == "331" ~ "Trade policies and regulations",
      SECTOR == "210" ~ "Transport and storage",
      SECTOR == "220" ~ "Communications",
      SECTOR == "230" ~ "Energy",
      SECTOR == "240" ~ "Banking and financial services",
      SECTOR == "250" ~ "Business and other services",
      SECTOR == "311" ~ "Agriculture",
      SECTOR == "312" ~ "Forestry",
      SECTOR == "313" ~ "Fishing",
      SECTOR == "321" ~ "Industry",
      SECTOR == "322" ~ "Mineral resources and mining",
      SECTOR == "332" ~ "Tourism"
    ),
    Units = paste("US dollar, Millions,", BASE_PER, "constant prices")
  ) %>%
  select(
    Year = TIME_PERIOD,
    Units,
    `Flow type`,
    Sector,
    Value = ObsValue,
  ) %>%
  mutate_at(c("Year", "Value"), as.numeric) %>%
  arrange(Year, `Flow type`, Sector)

oda_total <- oda %>%
  summarise(Value = sum(Value), .by = c(Year, Units, `Flow type`))

write.csv(oda_total, "./data/indicator_8-a-1.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
