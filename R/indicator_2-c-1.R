# Indicator 2.c.1 ---------------------------------------------------------
# Indicator of food price anomalies

options(timeout = 300) 
library(cansim)
library(tidyr)
library(dplyr)
library(stringr)
library(readsdmx)

primary_url <- "https://nsi-release-ro-statsuite.fao.org/rest/data/FAO,DF_AG_FPA_CFPI_2C1,1.0/A...124........?startPeriod=2015&dimensionAtObservation=AllDimensions"
FAO_Primary <- readsdmx::read_sdmx(primary_url)

data_final <-
  drop_na(FAO_Primary) %>%
  select(
    Year = TIME_PERIOD,
    Units = UNIT_MEASURE,
    Value = ObsValue
  ) %>% 
  mutate(
    Units = "Index"
  )

write.csv(
  data_final,
  "data/indicator_2-c-1.csv",
  row.names = FALSE,
  na = ""
)