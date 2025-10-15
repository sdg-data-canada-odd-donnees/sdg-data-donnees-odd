# Indicator 7.a.1 ---------------------------------------------------------
# International financial flows to developing countries in support of
# clean energy research and development and renewable energy production,
# including in hybrid systems

options(timeout = 300)
library(dplyr)
library(readsdmx)

# CRS: Creditor Reporting System (flows)
# See https://data-explorer.oecd.org/vis?tm=crs&pg=0&snb=25&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CRS%40DF_CRS&df[ag]=OECD.DCD.FSD&df[vs]=1.4&dq=CAN.DPGC.23210%2B23220%2B23230%2B23231%2B23232%2B23240%2B23250%2B23260%2B23270%2B23410%2B23631%2B23182.100._T._T.D.Q._T..&pd=2015%2C&to[TIME_PERIOD]=false
# Donor = Canada
# Recipient = Developing countries
# Sector = Energy research + Energy generation, renewable sources - multiple technologies +
#           Hydro-electric power plants + Solar energy for centralised grids +
#           Solar energy for isolated grids and standalone systems + Solar energy - thermal applications +
#           Wind energy + Marine energy + Geothermal energy + Biofuel-fired power plants +
#           Hybrid energy electric power plants + Electric power transmission and distribution (isolated mini-grids)
# Measure = Official Development Assistance
# Flow type = Disbursements, Price base = Constant prices

url <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/CAN.DPGC.23210+23220+23230+23231+23232+23240+23250+23260+23270+23410+23631+23182.100._T._T.D.Q._T..?startPeriod=2015&dimensionAtObservation=AllDimensions"

data_raw <- read_sdmx(url)

data <- data_raw %>%
  mutate(
    Sector = case_match(
      SECTOR,
      "23182" ~ "Energy research",
      "23210" ~ "Energy generation, renewable sources - multiple technologies",
      "23220" ~ "Hydro-electric power plants",
      "23230" ~ "Solar energy for centralised grids",
      "23231" ~ "Solar energy for isolated grids and standalone systems",
      "23232" ~ "Solar energy - thermal applications",
      "23240" ~ "Wind energy",
      "23250" ~ "Marine energy",
      "23260" ~ "Geothermal energy",
      "23270" ~ "Biofuel-fired power plants",
      "23410" ~ "Hybrid energy electric power plants",
      "23631" ~ "Electric power transmission and distribution (isolated mini-grids)"
    ),
    Units = paste("US dollar, Millions,", BASE_PER, "constant prices")
  ) %>%
  select(
    Year = TIME_PERIOD,
    Units,
    Sector,
    Value = ObsValue
  ) %>%
  mutate_at(c("Year", "Value"), as.numeric) %>%
  arrange(Year, Sector)

oda_total <- summarise(data, Value = sum(Value, na.rm = TRUE), .by = c(Year, Units))

data_final <- bind_rows(oda_total, data) %>%
  relocate(Sector, .before = Value)

write.csv(data_final, "./data/indicator_7-a-1.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
