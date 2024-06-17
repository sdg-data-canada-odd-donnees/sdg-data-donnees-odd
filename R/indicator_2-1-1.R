# Indicator 2.1.1 ---------------------------------------------------------
# 2.1.1 Prevalence of undernourishment

options(timeout = 300) 
library(cansim)
library(tidyr)
library(dplyr)
library(stringr)
library(devtools)
install_github("expersso/OECD")
library(OECD)

primary_url <- "https://nsi-release-ro-statsuite.fao.org/rest/data/FAO,DF_SN_ITK_DEFC_211,1.0/A...124........?startPeriod=2015&dimensionAtObservation=AllDimensions"
FAO_Primary <- readsdmx::read_sdmx(primary_url)

data_final <-
  drop_na(FAO_Primary) %>%
  select(
    Year = TIME_PERIOD,
    Units = UNIT_MEASURE,
    Value = ObsValue
  ) %>% 
  mutate(
    Units = "Percentage (%)",
    Value = str_remove(Value, '<')
  ) 
  
write.csv(
  data_final,
  "data/indicator_2-1-1.csv",
  row.names = FALSE,
  na = ""
)