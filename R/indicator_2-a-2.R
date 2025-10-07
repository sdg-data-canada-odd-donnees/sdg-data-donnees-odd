# Indicator 2.a.2 ---------------------------------------------------------
# Total official flows (official development assistance plus other official flows) to the agriculture sector

library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=crs&pg=0&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.DPGC.311.14%2B100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Donor = Canada
# Recipient = Developing countries
# Measure = Official Development Assistance (ODA) + Other Official Flows (non Export Credit)
# Sector = Agriculture
# Flow type = Disbursements, Price base = Constant prices

url_CRS_flows <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.311.14+100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

raw_data <- read_sdmx(url_CRS_flows)

ODA_OOF_flows_agriculture <- raw_data %>%
  select(
    Year = TIME_PERIOD,
    Units = BASE_PER,
    Measure = MEASURE,
    Value = ObsValue
  ) %>%
  mutate_at(c("Year", "Value"), as.numeric) %>%
  mutate(
    Measure = case_when(
      Measure == "100" ~ "Official Development Assistance",
      Measure == "14" ~ "Other Official Flows (non Export Credit)"
    ),
    # Get year base for USD prices from data
    Units = paste("US dollar, Millions,", Units, "constant prices"),
  ) %>%
  # Sort so ODA appears before OOF
  arrange(Measure, Year)

# Sum ODA and OOF flows
total <- ODA_OOF_flows_agriculture %>%
  summarise(Value = sum(Value), .by = c("Year", "Units"))

data_final <- bind_rows(ODA_OOF_flows_agriculture, total)

write.csv(data_final, "data/indicator_2-a-2.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
