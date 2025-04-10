# Indicator 2.a.2 ---------------------------------------------------------
# Total official flows (official development assistance plus other official flows) to the agriculture sector

library(dplyr)

url_CRS_flows <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.311.14+100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfile"

raw_data <- read.csv(url_CRS_flows)

# Force ObsValue to be numeric
raw_data$OBS_VALUE <- as.numeric(raw_data$OBS_VALUE)

ODA_OOF_flows_agriculture <- raw_data %>%
  select(
    Year = TIME_PERIOD,
    Units = BASE_PER,
    Measure = MEASURE,
    Value = OBS_VALUE
  ) %>%
  mutate(
    Measure = case_when(
      Measure == "100" ~ "Official Development Assistance",
      Measure == "14" ~ "Other Official Flows (non Export Credit)"
    ),
    # Get year base for USD prices from data
    Units = paste("US dollar, Millions,", Units, "constant prices"),
  ) %>%
  # Sort so ODA appears before OOF
  arrange(Measure)

# Sum ODA and OOF flows
total <- ODA_OOF_flows_agriculture %>%
  summarise(Value = sum(Value), .by = c("Year", "Units"))

data_final <- bind_rows(ODA_OOF_flows_agriculture, total)

write.csv(data_final, "data/indicator_2-a-2.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
