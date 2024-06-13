# Indicator 2.a.1 ---------------------------------------------------------
# The agriculture orientation index for government expenditures

options(timeout = 300) 
library(cansim)
library(dplyr)
library(stringr)
library(devtools)
install_github("expersso/OECD")
library(OECD)

primary_url <- "https://nsi-release-ro-statsuite.fao.org/rest/data/FAO,DF_AG_PRD_ORTIND_2A1,1.0/A...124........?startPeriod=2015&dimensionAtObservation=AllDimensions"
FAO_Primary <- readsdmx::read_sdmx(primary_url)

complementary_url_ag_gdp <- "https://nsi-release-ro-statsuite.fao.org/rest/data/FAO,DF_AG_PRD_AGVAS_2A1,1.0/A...124........?startPeriod=2015&dimensionAtObservation=AllDimensions"
FAO_Complementary_AG_GDP <- readsdmx::read_sdmx(complementary_url_ag_gdp)
  
complementary_url_ag_exp <- "https://nsi-release-ro-statsuite.fao.org/rest/data/FAO,DF_AG_XPD_AGSGB_2A1,1.0/A...124........?startPeriod=2015&dimensionAtObservation=AllDimensions"
FAO_Complementary_AG_Exp <- readsdmx::read_sdmx(complementary_url_ag_exp)

FAO_Primary_filtered <-
  FAO_Primary %>%
  select(
    Year = TIME_PERIOD,
    Series = SERIES_DESC,
    Units = UNIT_MEASURE,
    Value = ObsValue
  ) %>% 
  mutate(
    Units = "Index"
  )

FAO_Complementary_AG_GDP_filtered <-
  FAO_Complementary_AG_GDP %>%
  select(
    Year = TIME_PERIOD,
    Series = SERIES_DESC,
    Units = UNIT_MEASURE,
    Value = ObsValue
  ) %>% 
  mutate(
    Units = "Percentage (%)",
    Series = "Agriculture value added share of GDP"
  )

FAO_Complementary_AG_Exp_filtered <-
  FAO_Complementary_AG_Exp %>%
  select(
    Year = TIME_PERIOD,
    Series = SERIES_DESC,
    Units = UNIT_MEASURE,
    Value = ObsValue
  ) %>% 
  mutate(
    Units = "Percentage (%)",
    Series = "Agriculture share of Government Expenditure"
  )

data_final <- 
  bind_rows(FAO_Primary_filtered,FAO_Complementary_AG_GDP_filtered,FAO_Complementary_AG_Exp_filtered)

write.csv(
  data_final,
  "data/indicator_2-a-1.csv",
  row.names = FALSE,
  na = ""
)