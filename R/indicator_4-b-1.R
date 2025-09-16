## GIF 4.b.1

options(timeout = 300)

library(dplyr)
library(readsdmx)

url <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.110.100._T.E.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

scholarship_df <- read_sdmx(url)

data_final <-
  scholarship_df %>%
  select(
    Year = TIME_PERIOD,
    Value = ObsValue
  ) %>%
  mutate_all(as.numeric) %>%
  arrange(Year)

write.csv(
  data_final,
  "data/indicator_4-b-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
