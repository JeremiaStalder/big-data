## Script to request data from openaq api

#install.packages("httr")
#install.packages("jsonlite")

library(httr)
library(jsonlite)
library(RMariaDB)

#connect to the database, where the data will be safed
bigdatadb <- dbConnect(RMariaDB::MariaDB(), user='bdauser', password='BDA4ever!!!', dbname='bigdatadb', host='35.193.193.138')

#retrieve all available countries from the openaq api
call_country = "https://api.openaq.org/v1/countries?limit=10000"
countries = as.data.frame(fromJSON(content(GET(call_country), "text"), flatten = TRUE))

#retrieve all available locations from the openaq api
locations <- data.frame()
for (country in countries$results.code){
  call_country = paste("https://api.openaq.org/v1/locations?limit=10000&country[]=", country, sep = "")
  temp_data = as.data.frame(fromJSON(content(GET(call_country), "text"), flatten = TRUE))
  if (nrow(temp_data) >= 10000){
    print(country)
  }
  locations = rbind(locations, temp_data)
}
#wait so that not more than 2000 request per 5 minutes are send to the API
Sys.sleep(30)

#subfunction to check if a state exist in the google geocode api
checkifstate <- function(x){
  return(identical(x, c("administrative_area_level_1", "political")))
}

#retrive observations from the openaq api enrich this information with a state from the google geocode api
#for (j in 1:nrow(locations)){
for (j in 1:20){
  observations <- data.frame()
  #find the correct state based on the geolocation
  state = ""
  geo_call = paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=",locations[j,]$results.coordinates.latitude,",",locations[j,]$results.coordinates.longitude,"&language=en&key=AIzaSyDiSHaEP0q-6kXrliQKAZ3Rjm1p-47SL1E", sep="")
  geo_answer = fromJSON(content(GET(geo_call), "text"))
  if (geo_answer$status == "OK"){
    geo_data = geo_answer$results[sapply(geo_answer$results$types, checkifstate),]
    if (length(geo_data) > 0){
      geo_data = geo_data$address_components[[1]][sapply(geo_data$address_components[[1]]$types, checkifstate),]
      if (length(geo_data) > 0){
        state = geo_data$long_name[1]
      } else{
        print("No state found")
      } 
    }else{
      print("No state found")
    }
  }else{
    print("Google API Request failed")
  }
  
  #retrieve openaq information while adhere to the max limit of 10000 samples
  num_observations = 10000
  i = 1
  while (num_observations >= 10000){
    call_observation = paste("https://api.openaq.org/v1/measurements?limit=10000&location=", gsub(" ", "%20", locations[j,]$results.location), "&page=", i, sep = "")
    temp_data = fromJSON(content(GET(call_observation), "text"), flatten = TRUE)
    i = i+1
    if (length(temp_data$results) == 0){
      print(locations[j,]$results.location)
      print(temp_data$meta$found)
      num_observations = 0
    }else{
      temp_data$results$state = state
      num_observations = nrow(temp_data$results)
      observations = rbind(observations, temp_data$results)
    }
    #wait for 0.15 seconds so that not more than 2000 request per 5 minutes are send to the API
    Sys.sleep(0.15)
  }
  #aggregate data from the same date
  for (observation_date in observations){
    
  }
  #save the data in the sql database
  
}


call <- "https://api.openaq.org/v1/locations?limit=10000"
data <- GET(call)
data = content(data, "text")
data = as.data.frame(fromJSON(data, flatten = TRUE))