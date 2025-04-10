# Indicator 17.19.1 ---------------------------------------------------------
# Dollar value of all resources made available to strengthen statistical capacity in developing countries

options(timeout = 300)
library(dplyr)
library(jsonlite)
library(httr)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&pg=0&fc=Topic&bp=true&snb=25&vw=tb&df[ds]=dsDisseminateFinalCloud&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.3&dq=CAN.DPGC.16062.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&ly[cl]=TIME_PERIOD
# Measure = Official Development Assistance (ODA)
# Sector = Statistical capacity building
# Flow type = Disbursements, Price base = Constant prices

url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.16062.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfile"
statistics <- read.csv(url)

data_final <-
  statistics %>%
  select(
    Year = TIME_PERIOD,
    Value = OBS_VALUE
  )

write.csv(
  data_final,
  "data/indicator_17-19-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

