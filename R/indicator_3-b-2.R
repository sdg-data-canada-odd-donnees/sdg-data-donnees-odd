# Indicator 3.b.2 ---------------------------------------------------------
# Total net official development assistance to medical research and basic health sectors

options(timeout = 300)
library(dplyr)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&pg=0&fc=Topic&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.DPGC.12182%2B122.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Measure = Official Development Assistance (ODA)
# Sector = Medical research + Basic health
# Flow type = Disbursements, Price base = Constant prices
url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.122+12182.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfile"

CRS_raw <- read.csv(url)

CRS <- CRS_raw %>%
  mutate(
    `Assistance type` = case_when(
      SECTOR == "122" ~ "Basic health", # Official and private flows (headline)
      SECTOR == "12182" ~ "Medical research"
    ),
  ) %>%
  select(
    Year = TIME_PERIOD,
    `Assistance type`,
    Value = OBS_VALUE,
  ) %>%
  mutate(Value = as.numeric(Value))

CRS_total <- CRS %>%
  group_by(Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  mutate(`Assistance type` = NA) 

data_final <- bind_rows(CRS_total, CRS) %>%
  relocate(Value, .after = "Assistance type")

write.csv(data_final, "./data/indicator_3-b-2.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
