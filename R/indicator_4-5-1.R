## GIF 4.5.1
# Parity indices for education indicators

# load libraries
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)

# Fetch 4.5.1 data from UIS data using API
# See UIS Data API documentation: https://api.uis.unesco.org/api/public/documentation/
base_url <- "https://api.uis.unesco.org/api/public/data/indicators"

queryString <- list(
  # Fetch the following educational indicators:
  indicator = paste(
    # Completion rates
    "CR.1.GPIA",
    "CR.2.GPIA",
    "CR.3.GPIA",
    "CR.3.LPIA",
    "CR.3.WPIA",
    # Participation rate of youth and adults in formal and non-formal education and training in the previous 12 months
    "PRYA.12MO.AG15T64.GPIA",
    "PRYA.12MO.AG15T24.GPIA",
    "PRYA.12MO.AG25T54.GPIA",
    "PRYA.12MO.AG55T64.GPIA",
    # Proportion of students achieving at least a minimum proficiency level in reading
    "READ.PRIMARY.GPIA",
    "READ.LOWERSEC.GPIA",
    "READ.PRIMARY.LPIA",
    "READ.LOWERSEC.LPIA",
    "READ.PRIMARY.WPIA",
    "READ.LOWERSEC.WPIA",
    # Proportion of students achieving at least a minimum proficiency level in mathematics
    "MATH.PRIMARY.GPIA",
    "MATH.LOWERSEC.GPIA",
    "MATH.PRIMARY.LPIA",
    "MATH.LOWERSEC.LPIA",
    "MATH.PRIMARY.WPIA",
    "MATH.LOWERSEC.WPIA",
    # Educational attainment rates
    "EA.S1T8.AG25T99.GPIA",
    "EA.1T8.AG25T99.GPIA",
    "EA.2T8.AG25T99.GPIA",
    "EA.3T8.AG25T99.GPIA",
    "EA.4T8.AG25T99.GPIA",
    "EA.5T8.AG25T99.GPIA",
    "EA.6T8.AG25T99.GPIA",
    "EA.7T8.AG25T99.GPIA",
    "EA.8.AG25T99.GPIA",
    "EA.3T8.AG25T99.RUR.GPIA",
    "EA.3T8.AG25T99.URB.GPIA",
    "EA.4T8.AG25T99.RUR.GPIA",
    "EA.4T8.AG25T99.URB.GPIA",
    "EA.5T8.AG25T99.RUR.GPIA",
    "EA.5T8.AG25T99.URB.GPIA",
    "EA.6T8.AG25T99.RUR.GPIA",
    "EA.6T8.AG25T99.URB.GPIA",
    "EA.7T8.AG25T99.RUR.GPIA",
    "EA.7T8.AG25T99.URB.GPIA",
    "EA.8.AG25T99.RUR.GPIA",
    "EA.8.AG25T99.URB.GPIA",
    "EA.3T8.AG25T99.LPIA",
    "EA.4T8.AG25T99.LPIA",
    "EA.5T8.AG25T99.LPIA",
    "EA.6T8.AG25T99.LPIA",
    "EA.7T8.AG25T99.LPIA",
    "EA.8.AG25T99.LPIA",
    "EA.3T8.AG25T99.F.LPIA",
    "EA.3T8.AG25T99.M.LPIA",
    "EA.4T8.AG25T99.F.LPIA",
    "EA.4T8.AG25T99.M.LPIA",
    "EA.5T8.AG25T99.F.LPIA",
    "EA.5T8.AG25T99.M.LPIA",
    "EA.6T8.AG25T99.F.LPIA",
    "EA.6T8.AG25T99.M.LPIA",
    "EA.7T8.AG25T99.F.LPIA",
    "EA.7T8.AG25T99.M.LPIA",
    "EA.8.AG25T99.F.LPIA",
    "EA.8.AG25T99.M.LPIA",
    sep = ","
  ),
  geoUnit = "CAN" # fetch only data for Canada
)
# Send the request
response <- VERB("GET", base_url, query = queryString, add_headers("Accept-Encoding" = "gzip"), content_type("application/octet-stream"), accept("application/json"))

# Check if the response status code is 200 (OK)
if (status_code(response) == 200) {
  # If the response is successful, read the content
  resptext <- content(response, "text")
} else {
  # If the response is not successful, print the content and stop the execution with an error message
  print(content(response, "text"))
  stop("Failed to fetch data: ", status_code(response))
}

# Prepare JSON data retrieved from API.
data <- fromJSON(resptext)[["records"]] %>%
  filter(
    !(magnitude %in% c("NIL")),
    year >= 2015
  )

# Initialize empty vectors. These will hold series/unit/disaggregation details once data is parsed.
n <- nrow(data)
series <- vector(mode = "character", length = n)
units <- vector(mode = "character", length = n)
education_lvl <- vector(mode = "character", length = n)
age_group <- vector(mode = "character", length = n)
sex <- vector(mode = "character", length = n)
location <- vector(mode = "character", length = n)

# Split indicator IDs on "." characters to help with parsing in next step.
id <- str_split(data$indicatorId, "\\.")

for (i in 1:n) {
  # Parse completion rate parity indicators
  if (id[[i]][1] == "CR") {
    series[i] <- "Completion rate"
    if (id[[i]][2] == 1) {
      education_lvl[i] <- "Primary education"
    } else if (id[[i]][2] == 2) {
      education_lvl[i] <- "Lower secondary education"
    } else if (id[[i]][2] == 3) {
      education_lvl[i] <- "Upper secondary education"
    }

    # Parse educational attainment rate parity indicators
  } else if (id[[i]][1] == "EA") {
    series[i] <- "Educational attainment rate"
    if (id[[i]][2] == "S1T8") {
      education_lvl[i] <- "At least some primary (ISCED 1)"
    } else if (id[[i]][2] == "1T8") {
      education_lvl[i] <- "Completed primary education or higher"
    } else if (id[[i]][2] == "2T8") {
      education_lvl[i] <- "Completed lower secondary education or higher"
    } else if (id[[i]][2] == "3T8") {
      education_lvl[i] <- "Completed upper secondary education or higher"
    } else if (id[[i]][2] == "4T8") {
      education_lvl[i] <- "Completed post-secondary non-tertiary education or higher"
    } else if (id[[i]][2] == "5T8") {
      education_lvl[i] <- "Completed short-cycle tertiary education or higher"
    } else if (id[[i]][2] == "6T8") {
      education_lvl[i] <- "Completed Bachelor's or equivalent education or higher"
    } else if (id[[i]][2] == "7T8") {
      education_lvl[i] <- "Completed Master's or equivalent education or higher"
    } else if (id[[i]][2] == "8") {
      education_lvl[i] <- "Completed Doctoral or equivalent education"
    }
    if (id[[i]][4] == "RUR") {
      location[i] <- "Rural"
    } else if (id[[i]][4] == "URB") {
      location[i] <- "Urban"
    } else if (id[[i]][4] == "F") {
      sex[i] <- "Female"
    } else if (id[[i]][4] == "M") {
      sex[i] <- "Male"
    }

    # Parse participation rate in formal and non-formal training and education
  } else if (id[[i]][1] == "PRYA") {
    series[i] <- "Participation rate of youth and adults in formal and non-formal education and training in the previous 12 months"
    if (id[[i]][3] == "AG15T64") {
      age_group[i] <- "15-64 years old"
    } else if (id[[i]][3] == "AG15T24") {
      age_group[i] <- "15-24 years old"
    } else if (id[[i]][3] == "AG25T54") {
      age_group[i] <- "25-54 years old"
    } else if (id[[i]][3] == "AG55T64") {
      age_group[i] <- "55-64 years old"
    }

    # Parse % of students achieving min. proficiency level in reading
  } else if (id[[i]][1] == "READ") {
    series[i] <- "Proportion of students achieving at least a minimum proficiency level in reading"
    if (id[[i]][2] == "PRIMARY") {
      education_lvl[i] <- "Primary education"
    } else if (id[[i]][2] == "LOWERSEC") {
      education_lvl[i] <- "Lower secondary education"
    }

    # Parse % of students achieving min. proficiency level in math
  } else if (id[[i]][1] == "MATH") {
    series[i] <- "Proportion of students achieving at least a minimum proficiency level in mathematics"
    if (id[[i]][2] == "PRIMARY") {
      education_lvl[i] <- "Primary education"
    } else if (id[[i]][2] == "LOWERSEC") {
      education_lvl[i] <- "Lower secondary education"
    }
  }

  # Parse parity index type
  if (id[[i]][length(id[[i]])] == "GPIA") {
    units[i] <- "Adjusted gender parity index"
  } else if (id[[i]][length(id[[i]])] == "LPIA") {
    units[i] <- "Adjusted location parity index"
  } else if (id[[i]][length(id[[i]])] == "WPIA") {
    units[i] <- "Adjusted wealth parity index"
  }
}

# Insert series/units/disaggregation details into the dataframe.
data$Series <- series
data$Units <- units
data$`Highest level of education` <- education_lvl
data$`Age group` <- age_group
data$Sex <- sex
data$Location <- location

# Define function to transform values for progress calculation.
# Values inside the desired range [0.97, 1.03] are converted to 1 (ex: 1.01 --> 1).
# Values outside the range are measured by the distance to the nearest threshold (ex: 1.05 --> 1.02 and 0.95 --> 1.02)
progress_transform <- function(y) {
  if (y < 0.97) {
    return(1 / y)
  } else if (y > 1.03) {
    return(y)
  } else {
    return(1)
  }
}

final_data <- data %>%
  mutate(
    Progress = sapply(value, progress_transform),
    COMMENT_OBS = case_match(qualifier, "UIS_EST" ~ "Estimate produced by the UNESCO Institute for Statistics (UIS)."),
    value = round(value, 3)
  ) %>%
  select(
    Year = year,
    Series,
    Units,
    `Highest level of education`,
    `Age group`,
    Sex,
    Location,
    COMMENT_OBS,
    Progress,
    Value = value
  )

# Write the csv file
write.csv(final_data, "data/indicator_4-5-1.csv",
  na = "", row.names = FALSE, fileEncoding = "UTF-8"
)
