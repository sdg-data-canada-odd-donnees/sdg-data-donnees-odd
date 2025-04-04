## GIF 4.b.1

options(timeout = 300)

library(dplyr)

url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.110.100._T.E.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfile"

scholarship_df <- read.csv(url)

data_final <-
  scholarship_df %>%
  select(
    Year = TIME_PERIOD,
    Value = OBS_VALUE
  )

write.csv(
  data_final,
  "data/indicator_4-b-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)