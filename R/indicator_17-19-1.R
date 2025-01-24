# Indicator 17.19.1 ---------------------------------------------------------
# Dollar value of all resources made available to strengthen statistical capacity in developing countries

options(timeout = 300)
library(dplyr)
library(jsonlite)
library(httr)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CDevelopment%23DEV%23%7COfficial%20Development%20Assistance%20%28ODA%29%23DEV_ODA%23&pg=0&fc=Topic&bp=true&snb=25&vw=tb&df[ds]=dsDisseminateFinalCloud&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.3&dq=CAN.DPGC.16062.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&ly[cl]=TIME_PERIOD
# Measure = Official Development Assistance (ODA)
# Sector = Statistical capacity building
# Flow type = Disbursements, Price base = Constant prices

url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.16062.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
response <- GET(url)

if (status_code(response) == 200) {
  stat_CRS_json <- content(response)
  # Process the data as needed
} else {
  print(paste("Error:", status_code(response)))
}

# Extract observation
statistical <- stat_CRS_json$data$dataSets[[1]]$observations

# Convert observations to a more manageable format
# Extract the values and their corresponding keys
obs_keys <- names(statistical)
obs_values <- lapply(statistical, function(x) x[[1]])  # Access the first element in the list

# Create a data frame
statistical_df <- data.frame(
  Key = obs_keys,
  Value = sapply(obs_values, function(x) x[1])  # Extract the first value from the c() vector
)

# Convert Value to numeric
statistical_df$Value <- as.numeric(statistical_df$Value)

# Add a Year column starting from 2015
statistical_df$Year <- 2015 + seq(0, nrow(statistical_df) - 1)

data_final <-
  statistical_df %>%
  select(
    Year,
    Value
  )

write.csv(
  data_final,
  "data/indicator_17-19-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

