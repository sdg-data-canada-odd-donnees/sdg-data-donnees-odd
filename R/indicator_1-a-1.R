# Indicator 1.a.1 ---------------------------------------------------------
# 1.a.1 Total official development assistance grants from all donors that
#       focus on poverty reduction as a share of the recipient country’s 
#       gross national income

library(dplyr)
library(readsdmx)

# API URLs for ODA grants and GNI

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=19&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.1&dq=CAN.DPGC.52010%2B16050%2B140%2B122%2B112.11._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb&lc=en
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Basic education/Basic health/Water supply & sanitation/Multisector aid for basic social services/Food assistance, Measure = ODA Grants, Price base = Constant prices
url_crs_flows_odagrants <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.1/CAN.DPGC.52010+16050+140+122+112.11._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

# CRS: Creditor Reporting System (grant equivalent). 
# See https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&fs[1]=Donor%2C0%7CCanada%23CAN%23&fs[2]=Sector%2C4%7CAll%20sectors%231000%23%7CSector%20allocable%23450%23%7CSocial%20infrastructure%20and%20services%23100%23%7CEducation%23110%23%7CBasic%20education%23112%23&fs[3]=Sector%2C4%7CAll%20sectors%231000%23%7CSector%20allocable%23450%23%7CSocial%20infrastructure%20and%20services%23100%23%7CHealth%23120%23%7CBasic%20health%23122%23&fs[4]=Sector%2C3%7CAll%20sectors%231000%23%7CSector%20allocable%23450%23%7CSocial%20infrastructure%20and%20services%23100%23%7CWater%20supply%20%26%20sanitation%23140%23&fs[5]=Sector%2C4%7CAll%20sectors%231000%23%7CSector%20allocable%23450%23%7CSocial%20infrastructure%20and%20services%23100%23%7COther%20social%20infrastructure%20and%20services%23160%23%7CMultisector%20aid%20for%20basic%20social%20services%2316050%23&pg=0&fc=Flow%20type&snb=8&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_GREQ%40DF_CRS_GREQ&df[ag]=OECD.DCD.FSD&df[vs]=1.1&dq=CAN.DPGC.52010%2B112%2B122%2B140%2B16050.91._T._T..Q._T..&pd=%2C&to[TIME_PERIOD]=false&ly[cl]=TIME_PERIOD&ly[rw]=SECTOR&vw=tb
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Sector = Basic education/Basic health/Water supply & sanitation/Multisector aid for basic social services/Food assistance, Measure = ODA Grants, Price base = Constant prices
url_crs_grantequiv_odagrants <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_GREQ@DF_CRS_GREQ,1.1/CAN.DPGC.52010+112+122+140+16050.91._T._T..Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

# URL of source for GNI
# DAC1: Flows by donor (ODA+OOF+Private). See https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&fs[1]=Donor%2C0%7CCanada%23CAN%23&fs[2]=Measure%2C0%7CGross%20National%20Income%20%28GNI%29%231%23&pg=0&fc=Measure&snb=1&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_DAC1%40DF_DAC1&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.1....Q.&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb
# Applied filters: Donor = Canada, Measure = Gross National Income (GNI), Price base = Constant prices
url_gni <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.2/CAN.1....Q.?startPeriod=2015&dimensionAtObservation=AllDimensions" # source used by UK

oda_raw <- read_sdmx(url_crs_flows_odagrants)
gni_raw <- read_sdmx(url_gni)

if (any(sort(unique(oda_raw$BASE_PER)) != sort(unique(gni_raw$BASE_PER)))) {
  print("ERROR: Dollar amounts from different base years cannot be combined.")
  stop()
}

gni <- gni_raw %>%
  select(
    Year = TIME_PERIOD,
    GNI = ObsValue
  ) %>%
  mutate_at("GNI", as.numeric)

oda <- oda_raw %>%
  mutate(
    `Poverty reduction aid type` = case_match(
      SECTOR,
      "112" ~ "Basic education",
      "122" ~ "Basic health",
      "140" ~ "Water supply and sanitation",
      "16050" ~ "Multisector aid for basic social services",
      "52010" ~ "Food assistance"
    )
  ) %>%
  select(
    Year = TIME_PERIOD,
    `Poverty reduction aid type`,
    ODA = ObsValue,
  ) %>%
  mutate_at("ODA", as.numeric) %>%
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

# check difference between grant equivalent and flow values
oda_grantequiv_raw <- read_sdmx(url_crs_grantequiv_odagrants)

oda_grantequiv <- oda_grantequiv_raw %>%
  mutate(
    `Poverty reduction aid type` = case_match(
      SECTOR,
      "112" ~ "Basic education",
      "122" ~ "Basic health",
      "140" ~ "Water supply and sanitation",
      "16050" ~ "Multisector aid for basic social services",
      "52010" ~ "Food assistance"
    )
  ) %>%
  select(
    Year = TIME_PERIOD,
    `Poverty reduction aid type`,
    `ODA (grant equivalent)` = ObsValue,
  ) %>%
  arrange(Year) %>%
  mutate_at("ODA (grant equivalent)", as.numeric) %>%
  left_join(oda) %>%
  mutate(diff = ODA - `ODA (grant equivalent)`)
