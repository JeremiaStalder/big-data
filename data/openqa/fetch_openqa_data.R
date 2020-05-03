## Script to request data from openaq api

#install.packages("httr")
#install.packages("jsonlite")

library(httr)
library(jsonlite)

call_country = "https://api.openaq.org/v1/countries?limit=10000"
countries = as.data.frame(fromJSON(content(GET(call_country), "text"), flatten = TRUE))

locations <- data.frame()
for (country in countries$results.code){
  call_country = paste("https://api.openaq.org/v1/locations?limit=10000&country[]=", country, sep = "")
  temp_data = as.data.frame(fromJSON(content(GET(call_country), "text"), flatten = TRUE))
  if (nrow(temp_data) >= 10000){
    print(country)
  }
  locations = rbind(locations, temp_data)
}

observations <- data.frame()
for (location in locations$results.location){
  num_observations = 10000
  i = 1
  while (num_observations >= 10000){
    call_observation = paste("https://api.openaq.org/v1/measurements?limit=10000&location=", gsub(" ", "%20", location), "&page=", i, sep = "")
    temp_data = fromJSON(content(GET(call_observation), "text"), flatten = TRUE)
    num_observations = nrow(temp_data$results)
    i = i+1
    if (length(temp_data$results) == 0){
      print(location)
      print(temp_data$meta$found)
    }else{
      observations = rbind(observations, temp_data$results)
    }
  }
}


call <- "https://api.openaq.org/v1/locations?limit=10000"
data <- GET(call)
data = content(data, "text")
data = as.data.frame(fromJSON(data, flatten = TRUE))

Google Maps API-Key: AIzaSyDiSHaEP0q-6kXrliQKAZ3Rjm1p-47SL1E