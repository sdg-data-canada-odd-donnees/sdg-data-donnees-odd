# Indicator 2.a.2 ---------------------------------------------------------
# Total official flows (official development assistance plus other official flows) to the agriculture sector

library(dplyr)
library(readsdmx)

url_CRS_flows <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.1/CAN.DPGC.311.14+100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

raw_data <- read_sdmx(url_CRS_flows)

# Force ObsValue to be numeric
raw_data$ObsValue <- as.numeric(raw_data$ObsValue)

ODA_OOF_flows_agriculture <- raw_data %>%
  select(
    Year = TIME_PERIOD,
    Units = BASE_PER,
    Measure = MEASURE,
    Value = ObsValue
  ) %>%
  mutate(
    Measure = case_match(
      Measure,
      "100" ~ "Official Development Assistance",
      "14" ~ "Other Official Flows (non Export Credit)",
      .default = Measure
    ),
    Units = paste("US dollar, Millions,", Units, "constant prices"),
  )

# Sum ODA and OOF flows
total <- ODA_OOF_flows_agriculture %>%
  summarise(Value = sum(Value), .by = c("Year", "Units"))
  

data_final <- bind_rows(ODA_OOF_flows_agriculture, total)

write.csv(data_final, "data/indicator_2-a-2.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
