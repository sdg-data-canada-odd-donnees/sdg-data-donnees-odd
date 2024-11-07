## GIF 4.b.1

options(timeout = 300)

library(jsonlite)


scholarship_CRS_json <- fromJSON("https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.1/CAN.DPGC.110.100._T.E.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions")

# Extract observations
scholarship <- scholarship_CRS_json$data$dataSets$observations

# Convert observations to a more manageable format
# Extract the values and their corresponding keys
obs_keys <- names(scholarship)
obs_values <- lapply(scholarship, function(x) x[[1]])  # Access the first element in the list

# Create a data frame
scholarship_df <- data.frame(
  Key = obs_keys,
  Value = sapply(obs_values, function(x) x[1])  # Extract the first value from the c() vector
)

# Convert Value to numeric
scholarship_df$Value <- as.numeric(scholarship_df$Value)

# Add a Year column starting from 2015
scholarship_df$Year <- 2015 + seq(0, nrow(scholarship_df) - 1)

data_final <-
  scholarship_df %>%
  select(
    Year,
    Value
  )

write.csv(
  data_final,
  "data/indicator_4-b-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)