# Indicator 6.a.1 ---------------------------------------------------------
# Amount of water- and sanitation-related official development
# assistance that is part of a government-coordinated spending plan

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=crs&pg=0&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.DPGC.140%2B31140.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Recipient = Developing countries
# Sector = Water supply & sanitation + Agricultural water resources
# Measure = Official Development Assistance
# Flow type = Disbursements, Price base = Constant prices
url_oda <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.140+31140.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

oda_raw <- read_sdmx(url_oda)

oda <- oda_raw %>%
  mutate(
    Sector = case_when(
      SECTOR == "140" ~ "Water supply and sanitation",
      SECTOR == "31140" ~ "Agricultural water resources",
    ),
    Units = paste("US dollar, Millions,", BASE_PER, "constant prices")
  ) %>%
  select(
    Year = TIME_PERIOD,
    Units,
    Sector,
    Value = ObsValue,
  ) %>%
  mutate_at(c("Year", "Value"), as.numeric) %>%
  arrange(Year, Sector)

oda_total <- oda %>%
  summarise(Value = sum(Value), .by = c(Year, Units))

data_final <- bind_rows(oda, oda_total)

write.csv(data_final, "./data/indicator_6-a-1.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
