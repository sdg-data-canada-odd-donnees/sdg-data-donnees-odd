# Indicator 6.6.1 ------------------------------------------------------
# for wetland

library(dplyr)
library(cansim)

Land_cover <- get_cansim("38-10-0177-01", factors = FALSE)

# print(summary(Land_cover))

Wetland <- 
  Land_cover %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2015,
    GEO == "Canada",
    `Land cover class` == "Wetland (non-treed)"
  ) %>% 
  select(
    Year = REF_DATE,
    Value = VALUE
  )%>%  
  mutate(Year = as.numeric(Year))%>%
  mutate(Value = Value + 600000) # as indicate by SME (2026-04-29). "To that number, the treed wetland should be added. While we do not have design-based estimates for treed wetland, we have estimated them as approximately 600,000 square kilometres from the Land Cover Register: Geospatial files using pixel counting This would give a total wetland estimate of 1.87 million square kilometres."

  Wetland$Series = "Wetlands area"
  Wetland$Units = "Square kilometres"

# For river flow change
# data from here (entered manually): https://map.sdg661.app/

River_flow <- data.frame(Year=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
                             2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
                             2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
                             Water_extent=c("Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum",
                             "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Maximum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum",
                             "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum", "Minimum"),
                             Value=c(5794220.580591027, 6400420.032975022, 6690936.234687706, 5830594.576081903, 6575288.813537162, 6659884.579790524,
                             6172474.697782567, 6971150.213980398, 6874052.60151282, 7001953.283836924, 4730583.722334577, 6783873.832087756,
                             6561681.060878346, 7173895.083877676, 7034564.927548868, 5921674.822084858, 5989895.89970913, 6747597.849505357,
                             6748112.731914326, 6508511.369041173, 7728006.311350334, 7181544.785881384, 8531776.972213501, 5955387.472200596, 5123192.103100998, #Maximum ends here
                             1349996.8617709507, 1333484.9056345262, 1414108.8245655482, 1430451.3821097263, 1443210.0579541286, 1618471.7795527356,
                             1569787.6657257788, 1231118.335157755, 1555274.181134994, 1531112.7755389677, 1358865.0147916398, 1362974.92784245, 1331111.4961132945,
                             1375104.1553988988, 1618191.1582550092, 1513878.8207632871, 1449161.6961964518, 1752299.9954918378, 1525371.092360865, 1747148.3049383434,
                             1677967.1224933474, 1636442.4125579633, 1509139.9442102658, 1510459.9673157926, 1257208.4521212925))%>% 
  select(
    "Reservoir extent" = Water_extent,
    Year,
    Value)

River_flow$Series = "River flows"
River_flow$Units = "m3/s"

# final dataset

data_final <-
  bind_rows(Wetland, River_flow) %>% 
  select("Year", "Series", "Reservoir extent", "Units", "Value")

write.csv(
  data_final,
  "data/indicator_6-6-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)



