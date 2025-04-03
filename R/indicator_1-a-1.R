# Indicator 1.a.1 ---------------------------------------------------------
# 1.a.1 Total official development assistance grants from all donors that
#       focus on poverty reduction as a share of the recipient country’s 
#       gross national income

options(timeout = 300)
library(dplyr)
library(readsdmx)

# API URLs for ODA grants and GNI

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=19&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.1&dq=CAN.DPGC.52010%2B16050%2B140%2B122%2B112.11._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb&lc=en
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Basic education/Basic health/Water supply & sanitation/Multisector aid for basic social services/Development food assistance, Measure = ODA Grants, Price base = Constant prices
url_odagrants <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,/CAN.DPGC.112+122+140+520+16050.11._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfile"

# URL of source for GNI
# DAC1: Flows by donor (ODA+OOF+Private). See https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&fs[1]=Donor%2C0%7CCanada%23CAN%23&fs[2]=Measure%2C0%7CGross%20National%20Income%20%28GNI%29%231%23&pg=0&fc=Measure&snb=1&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_DAC1%40DF_DAC1&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.1....Q.&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb
# Applied filters: Donor = Canada, Measure = Gross National Income (GNI), Price base = Constant prices
url_gni <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.2/CAN.1....Q.?startPeriod=2015&dimensionAtObservation=AllDimensions" # source used by UK

oda_raw <- read.csv(url_odagrants)
gni_raw <- read_sdmx(url_gni)

# Stop and throw error if any dollar values use different base years
if (any(sort(unique(oda_raw$BASE_PER)) != sort(unique(gni_raw$BASE_PER)))) {
  print("ERROR: Dollar amounts from different base years cannot be combined.")
  stop()
}

gni <- gni_raw %>%
  select(
    Year = TIME_PERIOD,
    GNI = ObsValue
  ) %>%
  mutate_at("GNI", as.numeric) %>%
  mutate_at("Year", as.numeric)

oda <- oda_raw %>%
  mutate(
    `Poverty reduction aid type` = case_when(
      SECTOR == "112" ~ "Basic education",
      SECTOR == "122" ~ "Basic health",
      SECTOR == "140" ~ "Water supply and sanitation",
      SECTOR == "16050" ~ "Multisector aid for basic social services",
      SECTOR == "520" ~ "Development food assistance"
    )
  ) %>%
  select(
    Year = TIME_PERIOD,
    `Poverty reduction aid type`,
    ODA = OBS_VALUE,
  ) %>%
  mutate_at("ODA", as.numeric) %>%
  mutate_at("Year", as.numeric) %>%
  arrange(Year)

oda_total <- oda %>%
  summarise(ODA = sum(ODA), .by = c(Year))

data_final <- bind_rows(oda, oda_total) %>%
  # Calculate ODA as percentage of GNI
  left_join(gni, by = "Year") %>%
  mutate(Value = ODA / GNI * 100) %>%
  select(
    Year,
    `Poverty reduction aid type`,
    Value
  )

write.csv(data_final, "./data/indicator_1-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
