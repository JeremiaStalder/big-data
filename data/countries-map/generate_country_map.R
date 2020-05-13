## Script to generate mapping for state visualization data

setwd("D:/Programming/R/BigDataAnalytics/data/countries-map")

#install.packages("rgdal")
#install.packages("sp")
#install.packages("data.table")
#install.packages("RMariaDB")
#install.packages("tidyr")
#install.packages("shapefiles")
#install.packages("maptools")
#install.packages("rgeos")
#install.packages("raster")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("ggmap")
#install.packages("marmap")
#install.packages("lattice")
#install.packages("ggplot2")

library(sp)
library(rgdal)
library(data.table)
library(RMariaDB)
library(tidyr)
library(shapefiles)
library(maptools)
library(rgeos)
library(raster)
library(maps)
library(mapdata)
library(ggmap)
library(marmap)
library(lattice)
library(ggplot2)
library(dplyr)
library(data.table)

#define variables
dbuser='bdauser'
dbpassword='BDA4ever!!!'
dbname='bigdatadb'
dbhost='35.193.193.138'

# select the desired parameter
parameter = "pm10"

#read custom dictionary file
state_state_dict <- fread("data/state_gadm_dict_cleaned.csv", encoding="UTF-8")
state_state_dict = state_state_dict[, CountryCode := toupper(CountryCode)]
state_state_dict = separate_rows(state_state_dict, "sub_region_gadm", sep=";", convert=FALSE)

#get openaq data
bigdatadb <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)
#execute the query
res <- dbSendQuery(bigdatadb, paste0("SELECT country, state, AVG(value) FROM openaq WHERE parameter = '", parameter, "' GROUP BY country, state"))
openaq_data = dbFetch(res)
#close connection to database
dbClearResult(res)
dbDisconnect(bigdatadb)
colnames(openaq_data) <- c("country", "state", "value")

#read GADM data
gadm <-  readOGR("data/gadm36_levels_gpkg/gadm36_levels.gpkg", "level1", encoding="UTF-8", use_iconv=TRUE)
gadm_data <- as.data.table(gadm_state@data)
gadm_map <- fortify(gadm, region="NAME_1")

#save fortified file locally
con = file("data/gadm_fortified.csv","w", encoding="utf8")
write.csv(gadm_map, con, row.names=FALSE)
close(con)

#simplify poligons for faster plotting
#for(i in 1:length(gadm@polygons)){
# for(j in 1:length(gadm@polygons[[i]]@Polygons)){
#   temp <- as.data.frame(gadm@polygons[[i]]@Polygons[[j]]@coords)
#   names(temp) <- c("x", "y")
#   temp2 <- dp(temp, 0.01)
#   gadm@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
# }
#}

#create gadm_data
gadm_data = gadm_data[, CountryCode := toupper(substr(HASC_1, 1, 2))]
gadm_data = gadm_data[is.na(CountryCode), CountryCode := substr(GID_0, 1, 2)]
gadm_data = gadm_data[!is.na(NAME_1), NAME_1 := tolower(NAME_1)]
gadm_data = gadm_data[NAME_0 == "Mayotte", CountryCode := "YT"]
gadm_data = gadm_data[NAME_0 == "Poland", CountryCode := "PL"]
gadm_data = gadm_data[NAME_0 == "Northern Cyprus", CountryCode := "CY"]
gadm_data = gadm_data[NAME_0 == "Latvia", CountryCode := "LV"]
gadm_data = gadm_data[NAME_0 == "United States Minor Outlying Islands", CountryCode := "US"]

#prepare plot color vector
plot_color <- as.data.table(merge(openaq_data, state_state_dict, by.x =c("country", "state"), by.y = c("CountryCode", "state"), all.x = TRUE))
plot_color = plot_color[,.(value.mean=mean(value)), by=.(sub_region_gadm,country)]
plot_color = data.table(merge(gadm_data, plot_color, by.x=c("CountryCode", "NAME_1"), by.y = c("country", "sub_region_gadm"), all.x = TRUE))
plot_color = plot_color[is.na(value.mean), value.mean := 0]
plot_color = plot_color[, value.mean]

#plot worldmap
png(filename= paste0("plot_wolrd_", parameter, ".png"), width=10000, height=10000)
  plot(gadm, col = plot_color, border = 'darkgrey')
dev.off()

summary(plot_color)

                   
                   
                   
                   