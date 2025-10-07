# Indicator 17.19.1 ---------------------------------------------------------
# Dollar value of all resources made available to strengthen statistical capacity in developing countries

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=crs&pg=0&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.DPGC.16062.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Donor = Canada
# Recipient = Developing countries
# Measure = Official Development Assistance (ODA)
# Sector = Statistical capacity building
# Flow type = Disbursements, Price base = Constant prices

url <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.16062.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
statistics <- read_sdmx(url)

data_final <- statistics %>%
  mutate(
    Units = paste("US dollar, Millions,", BASE_PER, "constant prices")
  ) %>%
  select(
    Year = TIME_PERIOD,
    Units,
    Value = ObsValue
  ) %>%
  mutate_at(c("Year", "Value"), as.numeric) %>%
  arrange(Year)

write.csv(
  data_final,
  "data/indicator_17-19-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
