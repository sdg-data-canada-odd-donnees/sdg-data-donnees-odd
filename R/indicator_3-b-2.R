# Indicator 3.b.2 ---------------------------------------------------------
# Total net official development assistance to medical research and basic health sectors

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=crs&pg=0&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.DPGC.122%2B12182.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Measure = Official Development Assistance (ODA)
# Sector = Medical research + Basic health
# Flow type = Disbursements, Price base = Constant prices
url <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.122+12182.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

CRS_raw <- read_sdmx(url)

CRS <- CRS_raw %>%
  mutate(
    `Assistance type` = case_when(
      SECTOR == "122" ~ "Basic health", # Official and private flows (headline)
      SECTOR == "12182" ~ "Medical research"
    ),
    Units = paste("US dollar, Millions,", BASE_PER, "constant prices")
  ) %>%
  select(
    Year = TIME_PERIOD,
    Units,
    `Assistance type`,
    Value = ObsValue,
  ) %>%
  mutate_at(c("Year", "Value"), as.numeric) %>%
  arrange(Year, `Assistance type`)

CRS_total <- CRS %>%
  group_by(Year, Units) %>%
  summarise(Value = sum(Value, na.rm = TRUE))

data_final <- bind_rows(CRS_total, CRS) %>%
  relocate(`Assistance type`, .before = Value)

write.csv(data_final, "./data/indicator_3-b-2.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
