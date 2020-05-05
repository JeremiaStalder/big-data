## Script to request data from openaq api

#install.packages("httr")
#install.packages("jsonlite")
#install.packages("RMariaDB")
#install.packages("utf8")
#install.packages("data.table")

library(httr)
library(jsonlite)
library(RMariaDB)
library(utf8)
library(data.table)

#define variables
dbuser='bdauser'
dbpassword='BDA4ever!!!'
dbname='bigdatadb'
dbhost='35.193.193.138'
found <- 0
notworking <- 0
totalnumobservations <- 0
path = "data/9fc1e440-a83e-4d45-82a6-093e2c785907.csv"

#define functions
#function to reduce the length of a string
reduceLength <- function(x, maxlength){
  output = vector()
  for (element in x){
    if (nchar(element)>maxlength){
      output = append(output, substr(element, 1, maxlength))
    }else{
      output = append(output, element)
    }
  }
  return(output)
}

#function to check if a state exist in the google geocode api
checkifstate <- function(x){
  return(identical(x, c("administrative_area_level_1", "political")))
}
#function to add itation to a string
addCitationMarks <- function (x){
  x = gsub("'", "", x)
  return(paste("'", x, "'", sep =""))
}
#function to reformat date as string
formatDate <- function(x){
  return(format(x, "%Y-%m-%d"))
}
#function to remove special characters (prevent SQL error)
removeSpecialCharacters <-function(x){
  return(iconv(x, to="UTF-8"))
}
myAggregate <- function(x){
  if (typeof(x) == "double" || typeof(x) == "integer"){
    return(mean(x))
  }else{
    if (length(unique(x)) == 1){
      return(x[[1]])
    } else {
      return = ""
      for (element in x){
        if(!grepl(element, return, fixed=TRUE) && length(element) != 0){
          return = paste(return, element, sep= "; ") 
        }
      }
      return(return)
    }
  }
}

saveToDB <- function(data, db, tableName, columns, maxItems){
  #reformat columns
  for (column in columns){
    data[,column] = sapply(data[,column], addCitationMarks)
  }
  #generate query
  for (i in 1:ceiling(nrow(data)/maxItems)) {
    query = paste0('INSERT INTO ', tableName, ' (',paste0(colnames(data),collapse = ','),') VALUES ')
    vals = NULL
    for (j in 1:maxItems) {
      k = (i-1)*maxItems+j
      if (k <= nrow(data)) {
        vals[j] = paste0('(', paste0(data[k,],collapse = ','), ')')
      }
    }
    query = paste0(query, paste0(vals,collapse=','))
    #execute the query
    dbExecute(db, query)
  }
}

#load data from csv
data <- fread(path, verbose = TRUE)

#get data with them latitude/longitude and get geolocation
data[, geocode:=do.call(paste0,.SD), .SDcols=-1]

#retrieve all available countries from the openaq api
call_country = "https://api.openaq.org/v1/countries?limit=10000"
countries = as.data.frame(fromJSON(content(GET(call_country), "text"), flatten = TRUE))

#retrieve all available locations from the openaq api locations in china will be ignored
for (country in countries$results.code[ countries$results.code != "CN"]){
  print(country)
  locations <- data.frame()
  call_locations = paste("https://api.openaq.org/v1/locations?limit=10000&country[]=", country, sep = "")
  locations = as.data.frame(fromJSON(content(GET(call_locations), "text"), flatten = TRUE))
  if (nrow(locations) >= 10000){
    print(paste0(country, " has more than 10000 locations"))
  }
  #retrive observations from the openaq api enrich this information with a state from the google geocode api
  observations <- data.frame()
  for (j in 1:nrow(locations)){
    if (locations[j,]$results.coordinates.longitude != 0 && locations[j,]$results.coordinates.latitude != 0){
      found = found+1
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
            print("    No state found")
            state = country
          } 
        }else{
          print("    No state found")
          state = country
        }
      }else{
        print("    Google API Request failed")
        state = country
      }
      
      #retrieve openaq information while adhere to the max limit of 10000 samples
      num_observations = 10000
      i = 1
      while (num_observations >= 10000){
        call_observation = paste("https://api.openaq.org/v1/measurements?limit=10000&date_from=2000-01-01&location=", gsub(" ", "%20", locations[j,]$results.location), "&page=", i, sep = "")
        temp_data = fromJSON(content(GET(call_observation), "text"), flatten = TRUE)
        i = i+1
        if (length(temp_data$results) == 0){
          print(paste0("    ",locations[j,]$results.location))
          print(paste0("    ",temp_data$meta$found))
          notworking = notworking+1
          num_observations = 0
        }else{
          temp_data$results$state = state
          num_observations = nrow(temp_data$results)
          observations = rbind(observations, temp_data$results)
        }
        #wait for 0.15 seconds so that not more than 2000 request per 5 minutes are send to the API
        Sys.sleep(0.15)
        totalnumobservations = totalnumobservations + num_observations
      }
    } else {
      print("Location has no coordinates, it will be ignored")
    }
  }
  
  if (length(observations) != 0){
    #save raw data local as backup
    write.csv(observations, paste("D:/Programming/R/BigDataAnalytics/data/openaq/backup_data/",country, "_raw.csv"))
    observations = read.csv(paste("D:/Programming/R/BigDataAnalytics/data/openaq/backup_data/",country, "_raw.csv"))
    #aggregate data from the same date and the same state
    names(observations)[names(observations) == 'coordinates.longitude'] <- 'longitude'
    names(observations)[names(observations) == 'coordinates.latitude'] <- 'latitude'
    
    observations = observations[observations$value > 0, ]
    observations$date = sapply(as.POSIXct(observations$date.utc,tz='UTC'), formatDate)
    observations = subset(observations, select=-c(date.local, date.utc))
    observations$location = sapply(reduceLength( observations$location, 120), removeSpecialCharacters)
    observations$city = sapply(reduceLength( observations$city, 120), removeSpecialCharacters)
    observations$state = sapply(reduceLength( observations$state, 120), removeSpecialCharacters)
    observations_location = aggregate(observations[c("value", "state", "unit", "country", "latitude", "longitude")], by=list(location= observations$location, parameter = observations$parameter, city = observations$city, date = observations$date),FUN=myAggregate, drop = TRUE)
    observations_state = aggregate(observations[c("value", "latitude", "longitude", "unit")], by=list(state = observations$state, parameter = observations$parameter, country = observations$country, date = observations$date),FUN=myAggregate, drop = TRUE)
    
    #save processed data local as backup
    write.csv(observations_state, paste("D:/Programming/R/BigDataAnalytics/data/openaq/backup_data/",country, "_state.csv"))
    write.csv(observations_location, paste("D:/Programming/R/BigDataAnalytics/data/openaq/backup_data/",country, "_location.csv"))
    
    #open connection to database
    bigdatadb <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)
    
    #save location level data in the sql database
    print("write location sql")
    saveToDB(data= observations_location, db= bigdatadb, tableName = "openaq_location", columns= c("location", "state", "parameter","unit","country","city","date"), maxItems= 10000)
    
    #save state level data in the sql database
    print("write state sql")
    saveToDB(data= observations_state, db= bigdatadb, tableName = "openaq_state", columns= c("state", "parameter","unit","country","date"), maxItems= 10000)
    
    #close connection to database
    dbDisconnect(bigdatadb)
    
  } else {
    print(paste0("    No observations found for ", country))
  }
  print(paste0(notworking, " / ", found))
  print(totalnumobservations)
}