## Script to request data from openaq api

setwd("D:/Programming/R/BigDataAnalytics/data/openaq")

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
data_path = "data/9fc1e440-a83e-4d45-82a6-093e2c785907.csv"
isoDict_path = "data/wikipedia-iso-country-codes.csv"
save_path = "data/"

#define functions
#function to reduce the length of a string
reduceLength <- function(x, maxlength){
  output = vector()
  for (i in 1:length(x)){
    if (nchar(x[i])>maxlength){
      output = append(output, substr(x[i], 1, maxlength))
    }else{
      output = append(output, x[i])
    }
  }
  return(output)
}

#function to check if a state exist in the google geocode api
checkifstate <- function(x){
  return(identical(x, c("administrative_area_level_1", "political")))
}
checkifcountry <- function(x){
  return(identical(x, c("country", "political")))
}
#function to add itation to a string
addCitationMarks <- function (x){
  x = gsub("'", "", x)
  return(paste("'", x, "'", sep =""))
}

saveToDB <- function(data, db, tableName, maxItems){
  #reformat columns
  data[, country:=sapply(data[, country], addCitationMarks)]
  data[, location:=sapply(data[, location], addCitationMarks)]
  data[, date:=sapply(data[, date], addCitationMarks)]
  data[, parameter:=sapply(data[, parameter], addCitationMarks)]
  data[, unit:=sapply(data[, unit], addCitationMarks)]
  data[, city:=sapply(data[, city], addCitationMarks)]
  data[, state:=sapply(data[, state], addCitationMarks)]
  
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

getStateCountry <- function(x){
  apikey = "AIzaSyDiSHaEP0q-6kXrliQKAZ3Rjm1p-47SL1E"
  geo_call = paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=",as.double(x[["latitude"]]),",",as.double(x[["longitude"]]),"&language=en&key=", apikey, sep="")
  geo_answer = fromJSON(content(GET(geo_call), "text"))
  if (geo_answer$status == "OK"){
    geo_data_state = geo_answer$results[sapply(geo_answer$results$types, checkifstate),]
    if (length(geo_data_state) > 0){
      geo_data_state = geo_data_state$address_components[[1]][sapply(geo_data_state$address_components[[1]]$types, checkifstate),]
      if (length(geo_data_state) > 0){
        state  = geo_data_state$long_name[1]
      } else{
        print("No state found")
        state = x[["country.y"]]
      }
    }else{
      print("No state found")
      state  = x[["country.y"]]
    }
    geo_data_country = geo_answer$results[sapply(geo_answer$results$types, checkifstate),]
    if (length(geo_data_country) > 0){
      geo_data_country = geo_data_country$address_components[[1]][sapply(geo_data_country$address_components[[1]]$types, checkifcountry),]
      if (length(geo_data_country) > 0){
        country  = geo_data_country$short_name[1]
      } else{
        print("No country found")
        country = x[["country"]]
      }
    }else{
      print("No country found")
      country = x[["country"]]
    }
  }else{
    print("Google API Request failed")
    state  = x[["country.y"]]
    country = x[["country"]]
  }
  return(c(state, country))
}

getStateCountryTryCatch <- function(x){
  output  <- tryCatch(
        {output <- getStateCountry(x)},
        error = function(cond){
                  print("Encountered an error")
                  output <- c(x[["country.y"]],x[["country"]])},
        finally = {}
      )
  return(output)
}


#load data from csv
data <- fread(data_path, verbose = TRUE, encoding = "UTF-8")
isoCountryDict <-fread(isoDict_path, verbose = TRUE)
colnames(isoCountryDict) <- c("country", "iso2", "iso3", "numeric", "iso3166")
isoCountryDict = subset(isoCountryDict, select = c(country, iso2))

#clean data
data = data[location != "" & !is.na(location)]
data = data[!is.na(longitude) & !is.na(latitude)]
data = data[longitude != 0 & latitude != 0]
data[!is.na(country),country:= toupper(country)]

#replace unmatched countries
data[country == "BK",country:= "BA"]
data[country == "CE",country:= "LK"]
data[country == "CS",country:= "CR"]
data[country == "IZ",country:= "IQ"]
data[country == "KU",country:= "KW"]
data[country == "KV",country:= "XK"]
data[country == "TI",country:= "TJ"]
data[country == "TX",country:= "TM"]
data[country == "UC",country:= "CW"]
data[country == "VM",country:= "VN"]

#get data with them latitude/longitude and get geolocation
locations <- data[, .(latitude=mean(latitude), longitude=mean(longitude)), by=.(location, country)]
locations = merge(locations, isoCountryDict, by.x ="country", by.y = "iso2", all.x = TRUE)

#get state for locations
state = c()
country = c()
for (i in 1:nrow(locations)){
  stateCountry = getStateCountryTryCatch(locations[i,])
  state = append(state, stateCountry[1])
  country = append(country, stateCountry[2])
}
locations[ ,"state"] = state
locations[ ,"country2"] = country
locations = subset(locations, select = c(location, state, country, country2))

#add to main data.table
data = merge(data, locations, by.x =c("country","location"), by.y = c("country","location"))
print(paste0("Observations without a state: ", nrow(data[is.na(state)])))
fwrite(data, paste0(save_path, "df_by_location.csv"))

#remove not required column
data = subset(data, select = -c(country))
colnames(data)[length(colnames(data))] = "country"

#reduce value in primary key to adequate length
data[, "location"] = reduceLength( data[, "location"], 120)
data[, "city"] = reduceLength( data[, "city"], 120)
data[, "state"] = reduceLength( data[, "state"], 120)

#open connection to database
bigdatadb <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)

#save location level data in the sql database
print("write to sql database")
saveToDB(data= data, db= bigdatadb, tableName = "openaq", maxItems= 10000)

#close connection to database
dbDisconnect(bigdatadb)

print(paste0("Number of observations: ", nrow(data)))
print(paste0("Number of locations: ", nrow(locations)))


