## Script to generate mapping for state visualization data

setwd("D:/Programming/R/BigDataAnalytics/data/countries-map")

#install.packages("rgdal")
#install.packages("sp")
#install.packages("data.table")
#install.packages("RMariaDB")
#install.packages("translateR")

library(sp)
library(rgdal)
library(data.table)
library(RMariaDB)

#define variables
dbuser='bdauser'
dbpassword='BDA4ever!!!'
dbname='bigdatadb'
dbhost='35.193.193.138'


#open connection to database
bigdatadb <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)
#execute the query
res <- dbSendQuery(bigdatadb, "SELECT DISTINCT country, state FROM openaq")
openaq_countries = dbFetch(res)
#close connection to database
dbClearResult(res)
dbDisconnect(bigdatadb)

#get gadm data
gadm_data <-  as.data.table(readOGR("data/gadm36_levels_gpkg/gadm36_levels.gpkg", "level1", encoding="UTF-8", use_iconv=TRUE)@data)
gadm_data = gadm_data[, CountryCode := tolower(substr(HASC_1, 1, 2))]
gadm_data = gadm_data[is.na(CountryCode), CountryCode := tolower(substr(GID_0, 1, 2))]
gadm_data = gadm_data[NAME_0 == "Mayotte", CountryCode := "yt"]
gadm_data = gadm_data[NAME_0 == "Poland", CountryCode := "pl"]
gadm_data = gadm_data[NAME_0 == "Northern Cyprus", CountryCode := "cy"]
gadm_data = gadm_data[NAME_0 == "Latvia", CountryCode := "lv"]
gadm_data = gadm_data[NAME_0 == "United States Minor Outlying Islands", CountryCode := "us"]

#replace country state codes by state codes of the country 
#gadm_countries <- gadm_data[, paste(NAME_1, collapse = ", "), by = CountryCode]
#openaq_countries = merge(openaq_countries, gadm_countries, by.x= "sub_region_1", by.y= "CountryCode", all.x = TRUE)

#match different regions
gadm_data = gadm_data[, c("NAME_1", "CountryCode"), with=FALSE]
gadm_data = gadm_data[, NAME_1 := tolower(NAME_1)]
openaq_countries = openaq_countries[, CountryCode := tolower(CountryCode)]
gadm_data = gadm_data[, sub_region_gadm := NAME_1]
openaq_countries = merge(openaq_countries, gadm_data, by.x= c("state", "CountryCode"), by.y= c("NAME_1", "CountryCode"), all.x = TRUE)

#save locally
con = file("data/state_gadm_dict.csv","w", encoding="utf8")
write.csv(openaq_countries, con, row.names=FALSE)
close(con)

con = file("data/gadm_list.csv","w", encoding="utf8")
write.csv(gadm_data, con, row.names=FALSE)
close(con)

#files can be used for the manual matching