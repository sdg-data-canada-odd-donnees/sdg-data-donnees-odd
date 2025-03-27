# Indicator 15.a.1 ---------------------------------------------------------
# Official development assistance on conservation and sustainable use of biodiversity

options(timeout = 300)
library(dplyr)
library(jsonlite)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=biodiversity&pg=20&snb=32&vw=tb&df[ds]=dsDisseminateFinalCloud&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.1&dq=CAN.DPGC.41030.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Measure = Official Development Assistance (ODA)
# Sector = Biodiversity
# Flow type = Disbursements, Price base = Constant prices

biodiversity_CRS_json <- fromJSON("https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.41030.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions")

# Extract observations
biodiversity <- biodiversity_CRS_json$data$dataSets$observations

# Convert observations to a more manageable format
# Extract the values and their corresponding keys
obs_keys <- names(biodiversity)
obs_values <- lapply(biodiversity, function(x) x[[1]])  # Access the first element in the list

# Create a data frame
biodiversity_df <- data.frame(
  Key = obs_keys,
  Value = sapply(obs_values, function(x) x[1])  # Extract the first value from the c() vector
)

# Convert Value to numeric
biodiversity_df$Value <- as.numeric(biodiversity_df$Value)

# Add a Year column starting from 2015
biodiversity_df$Year <- 2015 + seq(0, nrow(biodiversity_df) - 1)

data_final <-
  biodiversity_df %>%
  mutate(
    Series = "Official development assistance on conservation and sustainable use of biodiversity"
  ) %>%
  select(
    Year,
    Series,
    Value
  )

write.csv(
  data_final,
  "data/indicator_15-a-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

