# Indicator 2.4.1 ---------------------------------------------------------
# 2.4.1 Proxy - Progress towards productive and sustainable agriculture

library(tidyr)
library(dplyr)
library(stringr)
library(readsdmx)

primary_url <- "https://nsi-release-ro-statsuite.fao.org/rest/data/FAO,DF_SDG_2_4_1_PROXY,1.0/A.AG_LND_SUST_PRXTS+AG_LND_SUST_PRXCSS.124.........?startPeriod=2015&dimensionAtObservation=AllDimensions"
FAO_Primary <- read_sdmx(primary_url)

data_final <-
  drop_na(FAO_Primary) %>%
  mutate(
    Units = "Score",
    SERIES = case_when(
      SERIES == "AG_LND_SUST_PRXTS" ~ "Trend score",
      SERIES == "AG_LND_SUST_PRXCSS" ~ "Current status score"
    )
  ) %>%
  select(
    Year = TIME_PERIOD,
    Series = SERIES,
    Units,
    Value = ObsValue
  )

write.csv(
  data_final,
  "data/indicator_2-4-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8",
)
