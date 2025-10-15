# Indicator 15.a.1 ---------------------------------------------------------
# Official development assistance on conservation and sustainable use of biodiversity

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=crs&pg=0&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.DPGC.41030.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Donor = Canada
# Recipient = Developing countries
# Measure = Official Development Assistance (ODA)
# Sector = Biodiversity
# Flow type = Disbursements, Price base = Constant prices

url <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.41030.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

biodiversity_df <- read_sdmx(url)

data_final <- biodiversity_df %>%
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
  "data/indicator_15-a-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
