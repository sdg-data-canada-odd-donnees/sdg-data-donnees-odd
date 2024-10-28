# Indicator 6.a.1 ---------------------------------------------------------
# 6.a.1 Amount of water- and sanitation-related official development 
# assistance that is part of a government-coordinated spending plan

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.1&dq=DAC..1000.100._T._T.D.Q._T..&lom=LASTNPERIODS&lo=5&to[TIME_PERIOD]=false&lc=en
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Water supply & sanitation/Agricultural water resources, Measure = Official Development Assistance, Price base = Constant prices
url_oda <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.1/DAC..31140+140.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

oda_raw <- read_sdmx(url_oda)

oda <- oda_raw %>%
  mutate(
    Sector = case_match(
      SECTOR,
      "140" ~ "Water supply and sanitation",
      "31140" ~ "Agricultural water resources",
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

data_final <- bind_rows(oda, oda_total)

write.csv(data_final, "./data/indicator_6-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
