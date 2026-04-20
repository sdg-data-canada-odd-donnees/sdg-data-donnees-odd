# Indicator 8.9.1 ------------------------------------------------------
# Tourism direct GDP as a proportion of total GDP and in growth rate

library(dplyr)
library(cansim)

# Petite fonction pour arrondir les 0.5 par en haut et non par en bas comme fait R. Trouvée ici : https://stackoverflow.com/questions/12688717/round-up-from-5
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

tourism_gdp <- get_cansim("36-10-0235-01", factors = FALSE)
# Check if last year in raw data is complete
# i.e. all quarters are available
if (substr(last(tourism_gdp$REF_DATE), 6, 7) != "10") {
  # If last year not complete, filter out last year
  tourism_gdp <- filter(tourism_gdp, REF_DATE < substr(max(REF_DATE), 1, 4))
}

data_final <- 
  tourism_gdp %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2015,
  ) %>% 
  select(
    REF_DATE,
    VALUE
  ) %>% 
  mutate(
    Year = substr(REF_DATE, 1, 4)  
  ) %>% 
  group_by(Year) %>%  
  summarise(data_final = round2((sum(VALUE)/4), 2), .groups = "drop") # en arrondissant les 0.5 par en haut
  #summarise(data_final = round((sum(VALUE)/4), 2), .groups = "drop") # en arrondissant normalement (0,5 par en bas)
  #summarise(data_final = sum(VALUE)/4, .groups = "drop") # en n'arrondissant pas

write.csv(
  data_final,
  "data/indicator_8-9-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

