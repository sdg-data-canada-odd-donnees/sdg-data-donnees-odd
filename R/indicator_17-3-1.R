# Indicator 17.3.1 ---------------------------------------------------------
# Foreign direct investment, official development assistance and South-South cooperation as a proportion of gross national income

library(dplyr)
library(readsdmx)

stop("Temporary skip 17.3.1")
# Indicator replaced in 2024. New indicator only requires FDI inflows.

# API URLs for ODA grants, FDI, and GNI

# DAC1: Flows by provider (ODA+OOF+Private)
# See https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&pg=0&fc=Topic&bp=true&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_DAC1%40DF_DAC1&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.11010..1160.USD.Q.&to[TIME_PERIOD]=false&ly[cl]=TIME_PERIOD&ly[rw]=SECTOR&pd=2015%2C
# Applied filters: Donor = Canada, Recipient = Developing countries, Flow type = Grant equivalents, Measure = Official Development Assistance (ODA), grant equivalent, Unit of measure = US Dollar, Price base = Constant prices
url_odagrants <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.4/CAN.11010..1160.USD.Q.?startPeriod=2014&dimensionAtObservation=AllDimensions"

# URL of source for GNI
# DAC1: Flows by donor (ODA+OOF+Private). See https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&fs[1]=Donor%2C0%7CCanada%23CAN%23&fs[2]=Measure%2C0%7CGross%20National%20Income%20%28GNI%29%231%23&pg=0&fc=Measure&snb=1&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_DAC1%40DF_DAC1&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.1....Q.&pd=2015%2C&to[TIME_PERIOD]=false&vw=tb
# Applied filters: Donor = Canada, Measure = Gross National Income (GNI), Price base = Constant prices
url_gni <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.2/CAN.1....Q.?startPeriod=2015&dimensionAtObservation=AllDimensions" # source used by UK

# URL of source for FDI
# See https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CFinance%20and%20investment%23FIN%23%7CForeign%20direct%20investment%20%28FDI%29%23FIN_FDI%23&fs[1]=Topic%2C2%7CEconomy%23ECO%23%7CExternal%20sector%23ECO_EXT%23%7CForeign%20direct%20investment%20%28FDI%29%23ECO_EXT_FDI%23&fs[2]=Reference%20area%2C0%7CCanada%23CAN%23&fs[3]=Frequency%20of%20observation%2C0%7CAnnual%23A%23&fs[4]=Measurement%20principle%2C0%7CDirectional%20principle%3A%20inward%23DI%23&fs[5]=FDI%20component%2C0%7CTotal%20direct%20investment%23D%23&pg=0&snb=12&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_FDI%40DF_FDI_CTRY_IND_SUMM&df[ag]=OECD.DAF.INV&df[vs]=1.0&dq=CAN.LE_FA_F.USD_EXC.DI.....W.._T.A.&pd=2015%2C&to[TIME_PERIOD]=false
# Applied filters: Reference area = Canada, Measure = FDI positions - total, Unit of measure = US dollars, exchange rate converted, US dollar, Millions, Measurement principle = Directional principle: inward, Counterpart area = World, Frequency of observation = Annual, Economic activity = Total - all activities
# This OECD table has the same data as the Statistics Canada Table 36-10-0008-01: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610000801
# The OECD table is preferred over the StatCan table primarily to avoid the necessity of currency conversions between US and Canadian dollars
url_fdi <- "https://sdmx.oecd.org/public/rest/data/OECD.DAF.INV,DSD_FDI@DF_FDI_CTRY_IND_SUMM,1.0/CAN.LE_FA_F.USD_EXC.DI.....W.._T.A.?startPeriod=2015&dimensionAtObservation=AllDimensions"

odagrants_raw <- read_sdmx(url_odagrants )
gni_raw <- read_sdmx(url_gni)
fdi_raw <- read_sdmx(url_fdi)

gni <- gni_raw %>%
  select(
    Year = TIME_PERIOD,
    GNI = ObsValue
  ) %>%
  mutate_at("GNI", as.numeric) %>%
  mutate_at("Year", as.numeric)

oda <- odagrants_raw %>%
  select(
    Year = TIME_PERIOD,
    ODA = ObsValue
  ) %>%
  mutate_at("ODA", as.numeric) %>%
  mutate_at("Year", as.numeric)

fdi <- fdi_raw %>%
  select(
    Year = TIME_PERIOD,
    FDI = ObsValue
  ) %>%
  mutate_at("FDI", as.numeric) %>%
  mutate_at("Year", as.numeric)

ODA_GNI <- 
  oda %>%
  # Calculate ODA as percentage of GNI
  left_join(gni, by = "Year") %>%
  mutate(Value = round(ODA / GNI * 100, 2)) %>%
  mutate(
    Series = "Official Development Assistance (ODA)"
  ) %>%
  select(
    Year,
    Series,
    Value
  ) %>%
  arrange(Year)

FDI_GNI <-
  fdi %>%
  # Calculate FDI as percentage of GNI
  left_join(gni, by = "Year") %>%
  mutate(Value = round(FDI / GNI * 100, 2)) %>%
  mutate(
    Series = "Foreign Direct Investment (FDI) Inflow"
  ) %>%
  select(
    Year,
    Series,
    Value
  ) %>%
  arrange(Year)

data_final <-
  bind_rows(FDI_GNI,ODA_GNI)

write.csv(
  data_final,
  "data/indicator_17-3-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
