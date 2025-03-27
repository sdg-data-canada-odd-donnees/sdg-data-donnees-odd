# Indicator 7.a.1 ---------------------------------------------------------
# International financial flows to developing countries in support of 
# clean energy research and development and renewable energy production, 
# including in hybrid systems

options(timeout = 300)
library(dplyr)
library(rjson)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=19&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.2&dq=CAN.DPGC.23410%2B23631%2B232%2B23182.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false&lc=en
# Applied filters: Donor = Canada, Recipient = Developing countries, Sector = Energy research/Energy generation, renewable sources/Hybrid energy electric power plants/Electric power transmission and distribution (isolated mini-grids), Measure = Official Development Assistance, Flow type = Disbursements, Price base = Constant prices

# Energy research
energy_url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.23182.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
energy_json <- fromJSON(file = energy_url)
energy_json_data <- t(as.data.frame(energy_json$data$dataSets[[1]]$observations))
energy_json_year <- as.data.frame(energy_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

energy_data <- bind_cols(energy_json_year, energy_json_data) %>%
  select(
    Year = `...1`,
    Sector = `...3`,
    Value = `...2`
  ) %>%
  mutate(
    Sector = "Energy research"
  )

# Energy generation, renewable sources
generation_url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.232.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
generation_json <- fromJSON(file = generation_url)
generation_json_data <- t(as.data.frame(generation_json$data$dataSets[[1]]$observations))
generation_json_year <- as.data.frame(generation_json$data$structure$dimensions$observation[[12]]$values) %>%
  select(
    starts_with("id")
  ) %>%
  t()

generation_data <- bind_cols(generation_json_year, generation_json_data) %>%
  select(
    Year = `...1`,
    Sector = `...3`,
    Value = `...2`
  ) %>%
  mutate(
    Sector = "Energy generation, renewable sources"
  )

# Hybrid energy electric power plants
hybrid_url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.23410.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
hybrid_json <- tryCatch({
  fromJSON(file = hybrid_url)
}, error = function(e) {
  NULL
})

# Only process hybrid data if JSON is not NULL and has observations
hybrid_data <- if (!is.null(hybrid_json) && 
                   length(hybrid_json$data$dataSets[[1]]$observations) > 0) {
  hybrid_json_data <- t(as.data.frame(hybrid_json$data$dataSets[[1]]$observations))
  
  hybrid_json_year <- as.data.frame(hybrid_json$data$structure$dimensions$observation[[12]]$values) %>%
    select(
      starts_with("id")
    ) %>%
    t()
  
  bind_cols(hybrid_json_year, hybrid_json_data) %>%
    select(
      Year = `...1`,
      Sector = `...3`,
      Value = `...2`
    ) %>%
    mutate(
      Sector = "Hybrid energy electric power plants"
    )
} else {
  NULL
}

# Electric power transmission and distribution (isolated mini-grids)
power_url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/CAN.DPGC.23631.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"
power_json <- tryCatch({
  fromJSON(file = power_url)
}, error = function(e) {
  NULL
})

# Only process power data if JSON is not NULL and has observations
power_data <- if (!is.null(power_json) && 
                  length(power_json$data$dataSets[[1]]$observations) > 0) {
  power_json_data <- t(as.data.frame(power_json$data$dataSets[[1]]$observations))
  
  power_json_year <- as.data.frame(power_json$data$structure$dimensions$observation[[12]]$values) %>%
    select(
      starts_with("id")
    ) %>%
    t()
  
  bind_cols(power_json_year, power_json_data) %>%
    select(
      Year = `...1`,
      Sector = `...3`,
      Value = `...2`
    ) %>%
    mutate(
      Sector = "Electric power transmission and distribution (isolated mini-grids)"
    )
} else {
  NULL
}

not_null_datasets <- list(energy_data, generation_data, hybrid_data, power_data)
not_null_datasets <- not_null_datasets[!sapply(not_null_datasets, is.null)]

# Bind all data sets

oda <- bind_rows(not_null_datasets) %>%
  mutate_at("Value", as.numeric) %>%
  arrange(Year)

oda_total <- oda %>%
  summarise(Value = sum(Value), .by = Year)

write.csv(oda_total, "./data/indicator_7-a-1.csv",
          row.names = FALSE, na = "", fileEncoding = "UTF-8")
