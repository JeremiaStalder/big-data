## Script to request data from openaq api

#install.packages("httr")
#install.packages("jsonlite")

library(httr)
library(jsonlite)
library(RMariaDB)

#define variables
dbuser='bdauser'
dbpassword='BDA4ever!!!'
dbname='bigdatadb'
dbhost='35.193.193.138'

#define functions
#function to check if a state exist in the google geocode api
checkifstate <- function(x){
  return(identical(x, c("administrative_area_level_1", "political")))
}
#function to add itation to a string
addCitationMarks <- function (x){
  return(paste("'", x, "'", sep =""))
}
#function to reformat date as string
formatDate <- function(x){
  return(format(x, "%Y-%m-%d"))
}


#retrieve all available countries from the openaq api
call_country = "https://api.openaq.org/v1/countries?limit=10000"
countries = as.data.frame(fromJSON(content(GET(call_country), "text"), flatten = TRUE))

#retrieve all available locations from the openaq api

for (country in countries$results.code){
  locations <- data.frame()
  call_country = paste("https://api.openaq.org/v1/locations?limit=10000&country[]=", country, sep = "")
  locations = as.data.frame(fromJSON(content(GET(call_country), "text"), flatten = TRUE))
  if (nrow(locations) >= 10000){
    print(country)
  }
  #retrive observations from the openaq api enrich this information with a state from the google geocode api
  for (j in 1:nrow(locations)){
    observations <- data.frame()
    observations_daily <- data.frame()
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
  }
  
  #save raw data local as backup
  write.csv(observations, paste("D:/Programming/R/BigDataAnalytics/data/openaq/backup_data/",country, ".csv"))
  
  #aggregate data from the same date and the same state
  observations$date = as.POSIXct(observations$date.utc,tz='UTC')
  observations = subset(observations, select=-c(date.local, date.utc))
  observations = observations[observations$value >= 0, ]
  observations_processed = aggregate(observations[c("value")], by=list(state = observations$state, parameter = observations$parameter, unit = observations$unit, country = observations$country, city = observations$city, date = observations$date, latitude = observations$coordinates.latitude, longitude = observations$coordinates.longitude),FUN=mean, drop = TRUE)
  
  #save processed data local as backup
  write.table(observations_processed, paste("D:/Programming/R/BigDataAnalytics/data/openaq/backup_data/",country, "_cleaned.csv"), row.names = F, col.names=F, sep='\t')
  
  #save the data in the sql database
  #reformat columns
  for (column in c("state", "parameter","unit","country","city")){
    observations_processed[,column] = sapply(observations_processed[,column], addCitationMarks)
  }
  observations_processed["date"] = sapply(sapply(observations_processed["date"], formatDate), addCitationMarks)
  
  #generate query
  query = paste0('INSERT INTO openaq (',paste0(colnames(observations_processed),collapse = ','),') VALUES ')
  vals = NULL
  for (j in 1:nrow(observations_processed)) {
    vals[j] = paste0("(", paste0(observations_processed[j,],collapse = ","), ")")
  }
  query = paste0(query, paste0(vals,collapse=','))
  
  #connect to the database, where the data will be safed and execute the query and close the connection afterwards
  bigdatadb <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)
  dbExecute(bigdatadb, query)
  dbDisconnect(bigdatadb)
}