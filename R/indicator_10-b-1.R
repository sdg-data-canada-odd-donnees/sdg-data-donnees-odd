# Indicator 10.b.1 ---------------------------------------------------------
# Total resource flows for development, by recipient and donor countries and type of flow (e.g. official development assistance, foreign direct investment and other flows)

options(timeout = 300)
library(dplyr)
library(readsdmx)

# DAC1: Flows by donor (ODA+OOF+Private)
# See https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&pg=0&fc=Topic&bp=true&snb=19&vw=tl&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_DAC1%40DF_DAC1&df[ag]=OECD.DCD.FSD&df[vs]=1.3&pd=2015%2C&ly[cl]=TIME_PERIOD&ly[rs]=SECTOR&ly[rw]=MEASURE&dq=CAN.5%2B200%2B415%2B1010%2B3000%2B3300..1140..Q.&to[TIME_PERIOD]=false
# Measure = Official and private flows/Official Development Assistance (ODA)/Other Official Flows (OOF)/Officially supported export credits/Private flows at market terms/Private grants, net
# Flow type = Disbursements, net, Price base = Constant prices
url <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.3/CAN.5+200+415+1010+3000+3300..1140..Q.?startPeriod=2015&dimensionAtObservation=AllDimensions"

flows_raw <- read_sdmx(url)

flows <- flows_raw %>%
  mutate(
    Measure = case_match(
      MEASURE,
      "5" ~ NA, # Official and private flows (headline)
      "1010" ~ "Official Development Assistance (ODA)",
      "200" ~ "Other Official Flows (OOF)",
      "3000" ~ "Officially supported export credits",
      "3300" ~ "Private flows at market terms",
      "415" ~ "Private grants, net"
    ),
    Units = paste("US dollar, Millions,", BASE_PER, "constant prices")
  ) %>%
  select(
    Year = TIME_PERIOD,
    Units,
    Measure,
    Value = ObsValue,
  ) %>%
  mutate_at(c("Year", "Value"), as.numeric) %>%
  arrange(Year, Measure)

write.csv(flows, "./data/indicator_10-b-1.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
