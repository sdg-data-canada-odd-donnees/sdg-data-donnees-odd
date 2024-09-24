# Indicator 1.a.1 ---------------------------------------------------------
# 1.a.1 Total official development assistance grants from all donors that
#       focus on poverty reduction as a share of the recipient country’s 
#       gross national income

library(dplyr)
library(readsdmx)

# URL of source for ODA grants
url_crs_flows <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.52010+16050+112+140+122.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
url_crs_grantequiv <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_GREQ@DF_CRS_GREQ,1.1/CAN.DPGC.112+122+140+16050+52010.100._T._T..Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

# URL of source for GNI
url_gni <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.2/CAN.1....Q.?startPeriod=2015&dimensionAtObservation=AllDimensions" # source used by UK


oda_raw <- read_sdmx(url_crs_grantequiv)
gni_raw <- read_sdmx(url_gni)

gni <- gni_raw %>%
  select(
    Year = TIME_PERIOD,
    GNI = ObsValue
  )

gni$GNI <- as.numeric(gni$GNI)

oda <- oda_raw %>%
  mutate(
    `Poverty reduction aid type` = case_match(
      SECTOR,
      "112" ~ "Basic education",
      "122" ~ "Basic health",
      "140" ~ "Water supply and sanitation",
      "16050" ~ "Multisector aid for basic social services",
      "52010" ~ "Development food aid"
    )
  ) %>%
  select(
    Year = TIME_PERIOD,
    `Poverty reduction aid type`,
    ODA = ObsValue,
  ) %>%
  arrange(Year)

oda$ODA <- as.numeric(oda$ODA)

oda_total <- oda %>%
  summarise(ODA = sum(ODA), .by = c(Year))

data_final <- bind_rows(oda, oda_total) %>%
  # Calculate ODA as percentage of GNI
  left_join(gni) %>%
  mutate(Value = ODA / GNI * 100) %>%
  select(
    Year,
    `Poverty reduction aid type`,
    Value
  )





