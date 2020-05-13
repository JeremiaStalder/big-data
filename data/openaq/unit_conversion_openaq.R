## Script to process and save weather data

setwd("D:/Programming/R/BigDataAnalytics/data/weather")

#install.packages("httr")
#install.packages("jsonlite")
#install.packages("RMariaDB")
#install.packages("utf8")
#install.packages("data.table")
#install.packages("stringr")
#install.packages("readr")
#install.packages("tidyverse")

library(httr)
library(jsonlite)
library(RMariaDB)
library(utf8)
library(data.table)
library(stringr)
library(readr)
library(tidyverse)

#define variables
dbuser='bdauser'
dbpassword='BDA4ever!!!'
dbname='bigdatadb'
dbhost='35.193.193.138'
found <- 0
notworking <- 0
totalnumobservations <- 0
data_path = "data/merged.csv"

#define functions
ppm_to_microgram <- function(particle,tempF, pressure_millibars_to_tenth, concentration_pmm){
  # convert ppm to microgramm/m3 according to https://www.ccohs.ca/oshanswers/chemicals/convert.html
  # inputs: temperature in Fahrenheit, pressure in millibars to tenth, concentration in ppm
  # output: concentraction in microgram/m3
  # Note: make sure inputs are cleaned. NAs are allowed
  
  # replace non-positive values with defaults for temperature, pressure, replace with NA for concentration_pmm
  tempF = replace_na(tempF,filter(constant_table, key== "subtract_f")$value + filter(constant_table, key== "temperature")$value / filter(constant_table, key== "factor_f")$value)
  tempC = (tempF-filter(constant_table, key== "subtract_f")$value)*filter(constant_table, key== "factor_f")$value
  tempC = ifelse(tempC>-filter(constant_table, key== "kelvin_to_celsius")$value , tempC, NA)
  
  
  pressure_mmHg = ifelse(pressure_millibars_to_tenth>0 , pressure_millibars_to_tenth * filter(constant_table, key== "convert_pressure")$value, NA)
  pressure_mmHg = replace_na(pressure_mmHg,filter(constant_table, key== "pressure")$value)
  
  concentration_pmm = ifelse(concentration_pmm>0 , concentration_pmm, NA)
  
  # calculate concentration
  volume_1_gram = filter(constant_table, key== "G")$value * (tempC+ filter(constant_table, key== "kelvin_to_celsius")$value)  / pressure_mmHg # calc volume
  mol_mass =  constant_table$value[match(particle, filter(constant_table, unit== "g/mol")$key)] # calc mol mass
  concentration_microgram_per_m3 = 10^3 * mol_mass * concentration_pmm / volume_1_gram # calc concentration 
  
  return(concentration_microgram_per_m3)
}



frshttConverter <- function(x){
  x = as.character(x)
  x = str_pad(x, 6, pad = "0")
  return(x)
}

#function to add itation to a string
addCitationMarks <- function (x){
  x = gsub("'", "", x)
  return(paste("'", x, "'", sep =""))
}

saveToDB <- function(data, tableName, maxItems){
  #reformat columns
  data[!is.na(country),country:= addCitationMarks(country)]
  data[!is.na(location),location:= addCitationMarks(location)]
  data[!is.na(date),date:= addCitationMarks(date)]
  data[!is.na(parameter),parameter:= addCitationMarks(parameter)]
  data[!is.na(unit),unit:= addCitationMarks(unit)]
  data[!is.na(city),city:= addCitationMarks(city)]
  data[!is.na(state),state:= addCitationMarks(state)]
  
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
    query = paste0(query, " ON DUPLICATE KEY UPDATE city = VALUES(city), unit = VALUES(unit), latitude = VALUES(latitude), longitude = VALUES(longitude), value = VALUES(value)")
    #open connection to database
    bigdatadb <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)
    
    #execute the query
    #print(query)
    print(paste0("Processed: ",i * maxItems, "/", nrow(data)))
    dbExecute(bigdatadb, query)
    
    #close connection to database
    dbDisconnect(bigdatadb)
    
  }
}


#load data from csv
data <- fread(data_path, verbose = TRUE, encoding = "UTF-8")
data[, c("LONGITUDE", "LATITUDE"):=NULL]

#merge data
colnames(data) = tolower(colnames(data))

#convert units
constant_table <<- read_delim("D:/Programming/R/BigDataAnalytics/data/unit_conversion/unit_conversion.csv", ";", escape_double = FALSE, col_types = cols(value = col_double()), locale = locale(decimal_mark = ","), trim_ws = TRUE)
data = data[unit == "ppm",value:= ppm_to_microgram(parameter, temp, stp, value)]
data = data[, unit:= "µg/m³"]

#clean data
data = data[!is.na(frshtt), frshtt_help:= frshttConverter(frshtt)]
data = data[, c("fog", "rain", "snow", "hail", "thunder", "tornado") := tstrsplit(frshtt_help, "", fixed=TRUE)]
data = data[, frshtt:=NULL]
data = data[, frshtt_help:=NULL]
data = data[, fog:=NULL]

#drop unneeded columns
data = data[, c("v1","temp_attributes", "dewp_attributes", "slp_attributes", "stp_attributes", "visib_attributes", "wdsp_attributes", "max_attributes", "min_attributes", "prcp_attributes"):=NULL]
data =data[, c("rain", "snow", "hail", "thunder", "tornado") := lapply(.SD, as.numeric), .SDcols = c("rain", "snow", "hail", "thunder", "tornado")]

#save locally
df <- setDF(data)
con = file("data/pollution_wheather_unified_units.csv","w", encoding="utf8")
write.csv(df, con, row.names=FALSE)
close(con)
#fwrite(data, paste0("data/pollution_wheather_unified_units.csv"))

#save location level data in the sql database
data = data[, c("country", "state", "location", "parameter", "city", "latitude", "longitude", "date", "value", "unit")]
print("write to sql database")
saveToDB(data= data, tableName = "openaq", maxItems= 10000)

