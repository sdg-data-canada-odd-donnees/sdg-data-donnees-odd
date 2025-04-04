# Indicator 15.a.1 ---------------------------------------------------------
# Official development assistance on conservation and sustainable use of biodiversity

options(timeout = 300)
library(dplyr)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=biodiversity&pg=20&snb=32&vw=tb&df[ds]=dsDisseminateFinalCloud&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.1&dq=CAN.DPGC.41030.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Measure = Official Development Assistance (ODA)
# Sector = Biodiversity
# Flow type = Disbursements, Price base = Constant prices

url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.41030.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfile"

biodiversity_df <- read.csv(url)

data_final <-
  biodiversity_df %>%
  select(
    Year = TIME_PERIOD,
    Value = OBS_VALUE
  )

write.csv(
  data_final,
  "data/indicator_15-a-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

