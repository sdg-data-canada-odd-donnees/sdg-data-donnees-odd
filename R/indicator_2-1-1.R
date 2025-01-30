# Indicator 2.1.1 ---------------------------------------------------------
# 2.1.1 Prevalence of undernourishment

library(tidyr)
library(dplyr)
library(stringr)
library(readsdmx)

primary_url <- "https://nsi-release-ro-statsuite.fao.org/rest/data/FAO,DF_SN_ITK_DEFC_211,1.0/A...124........?startPeriod=2015&dimensionAtObservation=AllDimensions"
FAO_Primary <- read_sdmx(primary_url)

data_final <-
  drop_na(FAO_Primary) %>%
  mutate(
    Units = "Percentage (%)",
    COMMENT_OBS_0 = case_match(ObsValue, "<2.5" ~ "Values showing as 2.5 may signify a prevalence of undernourishment below 2.5%"),
    COMMENT_OBS_1 = case_when(
      grepl("Estimates are based on projected values.", COMMENT_OBS, fixed = TRUE) ~ "Estimates are based on projected values.",
      # .default = "Estimated value"
      ),
    Value = as.numeric(str_remove(ObsValue, '<'))
  ) %>%
  select(
    Year = TIME_PERIOD,
    Units,
    Value,
    COMMENT_OBS_0,
    COMMENT_OBS_1
  )

write.csv(
  data_final,
  "data/indicator_2-1-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8",
)
