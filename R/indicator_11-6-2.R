# Indicator 11.6.2 ------------------------------------------------------ 
# Annual mean levels of fine particulate matter (e.g. PM2.5 and PM10) in cities (population weighted)

library(dplyr)
library(readr)
library(tidyr)

# Get data from source
national_data_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/air-quality/2023/2-pm25-average-national-2024-en.csv"

regional_data_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/air-quality/2023/3-pm25-average-regional-2024-en.csv"

city_data_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/air-quality/2023/Pollutant%20concentrations%20for%20selected%20Canadian%20urban%20areas%2c%202006-2020.csv"

# Fetch, load, and pre-process csv data from ECCC
national_raw <- read_csv(national_data_url, skip = 3, col_names = c("Year", "Value"), col_select = c(1,2))
national <- national_raw[1:(nrow(national_raw)-6),] %>% # remove last 6 rows (empty rows, notes, irrelevant data)
  mutate_at(c("Year", "Value"), as.numeric)

regional_raw <- read_csv(regional_data_url, skip = 5, col_names = FALSE, col_select = c(1,2,5,8,11,14))
colnames(regional_raw) <- c("Year", "Atlantic Canada", "Southern Quebec", "Southern Ontario", "Prairies and northern Ontario", "British Columbia")
regional <- regional_raw[1:(nrow(regional_raw)-6),] %>%
  gather(key = "Geography", value = "Value", -Year) %>%
  mutate_at(c("Year", "Value"), as.numeric)
  
city_raw <- read_csv(city_data_url, skip = 2, n_max = 26, na = "n/a")[-1,]
# Flip columns and rows
newcols = city_raw$`Urban region`
newrows = colnames(city_raw)[-1]
city_raw_transpose = data.frame(t(city_raw[-1]))
rownames(city_raw_transpose) <- NULL
colnames(city_raw_transpose) <- newcols
city <-  city_raw_transpose %>%
  mutate(Year = newrows) %>%
  gather(key = "Geography", value = "Value", -Year) %>%
  mutate_at(c("Year", "Value"), as.numeric)

# Combine and filter data
data_final <- bind_rows(national, regional, city) %>%
  filter(Year >= 2015) %>%
  relocate(Geography, .after = "Year")

# Write data to csv
write.csv(data_final, "data/indicator_11-6-2.csv", 
  na = "", row.names = FALSE, fileEncoding = "UTF-8")
