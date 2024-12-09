# Indicator 3.b.2 ---------------------------------------------------------
# Total net official development assistance to medical research and basic health sectors

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&pg=0&fc=Topic&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.DPGC.12182%2B122.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Measure = Official Development Assistance (ODA)
# Sector = Medical research + Basic health
# Flow type = Disbursements, Price base = Constant prices
url <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.2/CAN.DPGC.12182+122.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

CRS_raw <- read_sdmx(url)

CRS <- CRS_raw %>%
  mutate(
    `Assistance type` = case_match(
      SECTOR,
      "122" ~ "Basic health", # Official and private flows (headline)
      "12182" ~ "Medical research"
    ),
  ) %>%
  select(
    Year = TIME_PERIOD,
    `Assistance type`,
    Value = ObsValue,
  ) %>%
  mutate_at("Value", as.numeric) %>%
  arrange(Year, `Assistance type`)

CRS_total <- CRS %>%
  group_by(Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  mutate(`Assistance type` = "") 

data_final <-
  bind_rows(CRS_total, CRS)
  
write.csv(data_final, "./data/indicator_3-b-2.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
