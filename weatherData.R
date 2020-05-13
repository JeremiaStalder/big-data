library(geosphere)
library(data.table)
library(doSNOW)
library(rworldxtra) # used for high resolution of world map
library(sp) # used for conversion from lat and lon to ISO3
library(rworldmap) # used for conversion from lat and lon to ISO3
library(riem) # Package for accessing Weather Data from the Iowa Environment Mesonet
library(dplyr)
library(tidyverse)
library(RMariaDB)
library(microbenchmark) # for measuring performance of implemented functions
library(zoo)

# In case project isn't set up properly: sets up the working directory
# setwd("/home/jere/Dropbox/master/big_data/project/big-data")

#---------------------------------------------------- Functions --------------------------------------------------------------#
# takes a point (longitude, latitude) and returns a country ISO alpha3 code
# use resolution = 'low' for faster performance but less accuracy
coordsToISO3 = function(points) {
  countriesSP <- getMap(resolution = 'high')
  pointsSP = SpatialPoints(points, proj4string = CRS(proj4string(countriesSP)))
  indices = over(pointsSP, countriesSP)
  indices$ISO3
}

# imputes missing values with the mean of a column
impute.mean <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}

# removes outliers and sets them to a max/min value according to the quantiles
remove_outliers <- function(vect) {
  quantile1 <- quantile(vect, probs = c(.25, .75), na.rm = TRUE)
  quantile2 <- quantile(vect, probs = c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(vect, na.rm = TRUE)
  vect[vect < (quantile1[1] - H)] <- quantile2[1]
  vect[vect > (quantile1[2] + H)] <- quantile2[2]
  return(vect)
}

# calculates the haversine distance 
haversine <- function(p1, p2) {
  rad <- pi / 180
  p1 <- p1 * rad
  p2 <- p2 * rad
  lat <- p2[, 2] - p1[2]
  lon <- p2[, 1] - p1[1]
  cos(p1[2]) * cos(p2[, 2]) * sin(lon/2)^2 + sin(lat/2)^2
}

get_nearest <- function(point, p_matrix) {
  return(which.min(haversine(point, p_matrix)))
}

# function for importing a csv file
import_file <- function(x) {
  parsed_x <- fread(paste("./data/weather/WMOdata/",
                          x, sep = ""))
  return(parsed_x)
}

#---------------------------------------------------- Alternative weather data ----------------------------------------------------------#
# We now look at the weather data from the World Meterological Organisation: https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/readme.txt
# which was downloaded from: https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/

# get all filenames, there is a file for each station and year
filenames <- list.files("./data/weather/WMOdata")

# vectorize the function above
import_files <- Vectorize(import_file, SIMPLIFY = FALSE)

# apply the vectorized function to our csv file of all stations
observations <- import_files(filenames) %>% rbindlist()

# keep variables from observations which are needed
observations_cleaned <-
  observations[, c(
    "STATION",
    "NAME",
    "LATITUDE",
    "LONGITUDE",
    "DATE",
    "TEMP",
    "WDSP",
    "MAX",
    "MIN",
    "PRCP",
    "VISIB",
    "SNDP"
  )]
observations_cleaned <- observations

# Set every variable to the correct type
observations_cleaned$DATE <- as.Date(observations_cleaned$DATE)
observations_cleaned$DEWP <- as.numeric(observations_cleaned$DEWP)
observations_cleaned$TEMP <- as.numeric(observations_cleaned$TEMP)
observations_cleaned$SLP <- as.numeric(observations_cleaned$SLP)
observations_cleaned$STP <- as.numeric(observations_cleaned$STP)
observations_cleaned$VISIB <- as.numeric(observations_cleaned$VISIB)
observations_cleaned$MXSPD <- as.numeric(observations_cleaned$MXSPD)
observations_cleaned$WDSP <- as.numeric(observations_cleaned$WDSP)
observations_cleaned$MAX <- as.numeric(observations_cleaned$MAX)
observations_cleaned$MIN <- as.numeric(observations_cleaned$MIN)
observations_cleaned$PRCP <- as.numeric(observations_cleaned$PRCP)
observations_cleaned$SNDP <- as.numeric(observations_cleaned$SNDP)

# Set missing values (as of the official documentation) to NA values
observations_cleaned$TEMP <- na_if(observations_cleaned$TEMP, 9999.9)
observations_cleaned$WDSP <- na_if(observations_cleaned$WDSP, 999.9)
observations_cleaned$MAX <- na_if(observations_cleaned$MAX, 9999.9)
observations_cleaned$MIN <- na_if(observations_cleaned$MIN, 9999.9)
observations_cleaned$PRCP <- na_if(observations_cleaned$PRCP, 99.9)
observations_cleaned$DEWP <- na_if(observations_cleaned$DEWP, 9999.9)
observations_cleaned$SLP <- na_if(observations_cleaned$SLP, 999.9)
observations_cleaned$STP <- na_if(observations_cleaned$STP, 999.9)
observations_cleaned$VISIB <- na_if(observations_cleaned$VISIB, 999.9)
observations_cleaned$MXSPD <- na_if(observations_cleaned$MXSPD, 999.9)
observations_cleaned$SNDP <- na_if(observations_cleaned$SNDP, 999.9)

# STATION: station number
# NAME: name of the station
# LATITUDE: latitude of the station
# LONGITUDE: longitude of the station
# DATE: date
# TEMP: mean temperature of day in Fahrenheit
# WDSP: mean windspeed of the day in knots
# MAX: maximum temperature of the day
# MIN: minimum temperature of the day
# PRCP: total precipitation in inches
# DEWP:
# SLP:
# STP:
# VISIB:
# MXSPD:
# SNDP:

# Store the weather data
fwrite(observations_cleaned, "./data/weather/all_observations.csv", sep = ",")

weather <- fread("./data/weather/all_observations.csv")
openaq <- fread("./data/weather/openaq.csv")

# we round the longitudes and latitudes to the 2nd digit (ca. 1.1km) after the comma, this is more than enough for our purpose
weather$LONGITUDE <- round(weather$LONGITUDE, 2)
weather$LATITUDE <- round(weather$LATITUDE, 2)
weather$date <- weather$DATE
openaq$longitude <- round(openaq$longitude, 2)
openaq$latitude <- round(openaq$latitude, 2)


# Create points from lon and lat from weather observations
weather_points <- data.table(lon = c(weather$LONGITUDE),
                             lat = c(weather$LATITUDE)) %>% distinct()

# create a matrix, for slight performance increase
weather_p_matrix <- as.matrix(weather_points[, 1:2])

# Create points from lon and lat from airquality observations
aq_points <- data.table(lon = c(openaq$longitude),
                        lat = c(openaq$latitude)) %>% distinct()

# create a matrix for airquality points
aq_p_matrix <- as.matrix(aq_points[, 1:2])

# match the location of the airquality stations to the closest weather station, 
# we do this by applying the get_nearest function, which calls the haversine function,
# calculating the closest point on the earth
aqToWeather <- apply(aq_p_matrix, 1, get_nearest, p_matrix = weather_p_matrix)

# bind the the longitude and latitude of the weather stations to the locations of the airquality stations
# can be understood as binding the keys from both tables to each other
w_p <- matrix(data = NA,
              nrow = length(aqToWeather),
              ncol = 2)
for (i in seq(1, length(aqToWeather), 1)) {
  w_p[i, 1] <- weather_points[aqToWeather[i], ]$lon
  w_p[i, 2] <- weather_points[aqToWeather[i], ]$lat
}
mergeTable <- cbind(aq_points, w_p)
colnames(mergeTable) <- c("longitude", "latitude", "LONGITUDE", "LATITUDE")

# merge the airquality table with the matching table
merged_aq <- merge(openaq, mergeTable, by = c("longitude", "latitude"))
# remove the unused table
rm(openaq)

# Finally, fully merge the weather observations to the airquality observations
merged <- merge(merged_aq, weather, by = c("LONGITUDE", "LATITUDE", "date"))
merged[, DATE := NULL]

# Save the merged data for later usage
fwrite(merged, "./data/weather/pollution_wheather_unified_units.csv", sep = ",")


#---------------------------------------- Make predictions of pollution based on weather -----------------------------------

merged <- fread("./data/weather/pollution_wheather_unified_units.csv")

# remove colums that certainly will not be used
clean <- merged[,-c(2:4, 6, 8, 10, 11, 12, 17, 25)]
rm(merged)
length(unique(clean$state[clean$date > "2010-01-01"]))
# impute the missing data
features = c("wdsp", "mxspd", "max", "min")
clean[, (features) := lapply(.SD, impute.mean), by = state, .SDcols = features]

#drop rows with NA
clean = clean[complete.cases(clean)]

# get all parameter names
particles <- unique(clean_openaq$parameter)
#  "o3"   "co"   "no2"  "pm10" "so2"  "pm25" "bc"

o3_train <- filter(clean, parameter == "o3", date < "2020-01-01")
o3_pred <- filter(clean, parameter == "o3", date >= "2019-01-01")
o3_train$value <- remove_outliers(o3_train$value)
fit <-
  lm(
    "value ~ temp + dewp + slp + wdsp + mxspd + prcp + rain + max + min + snow + hail + thunder + tornado",
    o3_train
  )
predict <- predict(fit, newdata = o3_pred)
o3_prediction <-
  data.table(date = o3_pred$date,
             state = o3_pred$state,
             prediction = predict)
o3_prediction <-
  o3_prediction %>% group_by(state = state, date = date) %>% summarise(prediction = mean(prediction))
o3_prediction$prediction[o3_prediction$prediction < 0] <- 0
  fwrite(o3_prediction, "./data/predictionAirpollutionFromWeatherData/o3_prediction.csv", sep = ",")
  
  co_train <- filter(clean, parameter == "co", date < "2020-01-01")
  co_pred <- filter(clean, parameter == "co", date >= "2019-01-01")
  co_train$value <- remove_outliers(co_train$value)
  fit <-
    lm(
      "value ~ temp + dewp + slp + wdsp + mxspd + prcp + rain + max + min + snow + hail + thunder + tornado",
      co_train
    )
  predict <- predict(fit, newdata = co_pred)
  co_prediction <-
    data.table(date = co_pred$date,
               state = co_pred$state,
               prediction = predict)
  co_prediction <-
    co_prediction %>% group_by(state = state, date = date) %>% summarise(prediction = mean(prediction))
  co_prediction$prediction[co_prediction$prediction < 0] <- 0
  fwrite(co_prediction, "./data/predictionAirpollutionFromWeatherData/co_prediction.csv", sep = ",")

no2_train <-
  filter(clean, parameter == "no2", date < "2020-01-01")
no2_pred <-
  filter(clean, parameter == "no2", date >= "2019-01-01")
no2_train$value <- remove_outliers(no2_train$value)
fit <-
  lm(
    "value ~ temp + dewp + slp + wdsp + mxspd + prcp + rain + max + min + snow + hail + thunder + tornado",
    no2_train
  )
predict <- predict(fit, newdata = no2_pred)
no2_prediction <-
  data.table(date = no2_pred$date,
             state = no2_pred$state,
             prediction = predict)
no2_prediction <-
  no2_prediction %>% group_by(state = state, date = date) %>% summarise(prediction = mean(prediction))
no2_prediction$prediction[no2_prediction$prediction < 0] <- 0
fwrite(no2_prediction, "./data/predictionAirpollutionFromWeatherData/no2_prediction.csv", sep = ",")

so2_train <-
  filter(clean, parameter == "so2", date < "2020-01-01")
so2_pred <-
  filter(clean, parameter == "so2", date >= "2019-01-01")
so2_train$value <- remove_outliers(so2_train$value)
fit <-
  lm(
    "value ~ temp + dewp + slp + wdsp + mxspd + prcp + rain + max + min + snow + hail + thunder + tornado",
    so2_train
  )
predict <- predict(fit, newdata = so2_pred)
so2_prediction <-
  data.table(date = so2_pred$date,
             state = so2_pred$state,
             prediction = predict)
so2_prediction <-
  so2_prediction %>% group_by(state = state, date = date) %>% summarise(prediction = mean(prediction))
so2_prediction$prediction[so2_prediction$prediction < 0] <- 0
fwrite(so2_prediction, "./data/predictionAirpollutionFromWeatherData/so2_prediction.csv", sep = ",")


pm10_train <-
  filter(clean, parameter == "pm10", date < "2020-01-01")
pm10_pred <-
  filter(clean, parameter == "pm10", date >= "2019-01-01")
pm10_train$value <- remove_outliers(pm10_train$value)
fit <-
  lm(
    "value ~ temp + dewp + slp + wdsp + mxspd + prcp + rain + max + min + snow + hail + thunder + tornado",
    pm10_train
  )
predict <- predict(fit, newdata = pm10_pred)
pm10_prediction <-
  data.table(date = pm10_pred$date,
             state = pm10_pred$state,
             prediction = predict)
pm10_prediction <-
  pm10_prediction %>% group_by(state = state, date = date) %>% summarise(prediction = mean(prediction))
pm10_prediction$prediction[pm10_prediction$prediction < 0] <- 0
fwrite(pm10_prediction, "./data/predictionAirpollutionFromWeatherData/pm10_prediction.csv", sep = ",")

# Predict pm25 parameter
pm25_train <-
  filter(clean, parameter == "pm25", date < "2020-01-01")
pm25_pred <-
  filter(clean, parameter == "pm25", date >= "2019-01-01")
pm25_train$value <- remove_outliers(pm25_train$value)
fit <-
  lm(
    "value ~ temp + dewp + slp + wdsp + mxspd + prcp + rain + max + min + snow + hail + thunder + tornado",
    pm25_train
  )
predict <- predict(fit, newdata = pm25_pred)
pm25_prediction <-
  data.table(date = pm25_pred$date,
             state = pm25_pred$state,
             prediction = predict)
pm25_prediction <-
  pm25_prediction %>% group_by(state = state, date = date) %>% summarise(prediction = mean(prediction))
pm25_prediction$prediction[pm25_prediction$prediction < 0] <- 0
fwrite(pm25_prediction, "./data/predictionAirpollutionFromWeatherData/pm25_prediction.csv", sep = ",")


#---------------------------------------------------- Visualisation of Germany -----------------------

merged <- fread("./data/weather/pollution_wheather_unified_units.csv")

# remove colums that certainly will not be used
clean <- merged[,-c(2:3, 6, 8, 10, 11, 12, 17, 25)]
rm(merged)

# o3 good for temp
germany_pm25 <- filter(clean, country == "DE", date > "2018-01-01", parameter == "pm25") %>% group_by(date = date) %>% summarise_if(is.numeric, mean, na.rm = TRUE)

germany_rolling_pm25 <- germany_pm25 %>% arrange(date) %>% mutate( value = rollapply(value, 30, mean, align = "center", fill = NA),
    wdsp = rollapply(wdsp, 30, mean, align = "center", fill = NA)) %>% ungroup()


germany_rolling_pm25 <- na.omit(germany_rolling_pm25)
germany_rolling_pm25$date <- as.Date(germany_rolling_pm25$date)

ggplot(germany_rolling_pm25, aes(date)) + 
  geom_line(aes(y = value, colour = "pm25 in µg/m³")) + 
  geom_line(aes(y = wdsp, colour = "wind speed"))+
  theme_bw()+
  ggtitle("30 day rolling average of temperature and pm25 particles") +
  ylab(label= "wind speed and pm25 particles") +
  theme(legend.title = element_blank()) +
                theme(legend.position = "bottom") + 
                theme(plot.title = element_text(size=10, face="bold"))+
                theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10,face="bold")) + 
                ggsave(file="./presentation_charts/windspeed_pm25_germany.png", width=6, height=4, dpi=600)


germany_o3 <- filter(clean, country == "DE", date > "2018-01-01", parameter == "o3") %>% group_by(date = date) %>% summarise_if(is.numeric, mean, na.rm = TRUE)

germany_rolling_o3 <- germany_o3 %>% arrange(date) %>% mutate( value = rollapply(value, 30, mean, align = "center", fill = NA),
    temp = rollapply(temp, 30, mean, align = "center", fill = NA),
    min = rollapply(min, 30, mean, align = "center", fill = NA),
    max = rollapply(max, 30, mean, align = "center", fill = NA)) %>% ungroup()


germany_rolling_o3 <- na.omit(germany_rolling_o3)
germany_rolling_o3$date <- as.Date(germany_rolling_o3$date)

ggplot(germany_rolling_o3, aes(date)) + 
  geom_line(aes(y = value, colour = "o3 in µg/m³")) + 
  geom_line(aes(y = temp, colour = "average temperature in fahrenheit"))+
  theme_bw()+
  ggtitle("30 day rolling average of temperature and o3 particles") +
  ylab(label= "temperature and o3 particles") +
  theme(legend.title = element_blank()) +
                theme(legend.position = "bottom") + 
                theme(plot.title = element_text(size=10, face="bold"))+
                theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10,face="bold")) +
               ggsave(file="./presentation_charts/temperature_o3_germany.png", width=6, height=4, dpi=600)

#---------------------------------------------------- Alternative weather data ----------------------------------------------------------#
# The following part of the code fetches the weather data from the NOOA networks, this is a network of weather stations at airports.
# Fetch all possible NOOA networks

# station: station name
# valid: timestamp of observation
# tmpf: Air temperature in Fahrenheit
# relh: Relative Humidity in %
# sknt: Wind Speed in knots
# p01i: precipitation

# See https://mesonet.agron.iastate.edu/request/download.phtml for more detailed documentation

networks <- riem_networks()

# Initialize dataframe
observations <- data.frame()

# Iterate through all networks, usually there is one network per country or region
for (net in networks$code) {
  stations <- riem_stations(net) # stations per network
  count <- 0 # counter for number of iterations
  # Iterate through all stations in a network
  for (stat in stations$id) {
    count <- count + 1
    if (count >= 0) {
      # fetch the observations of the current station
      current_observation <-
        riem_measures(stat, date_start = "2016-01-01")
      
      # only look at stations with more than 1500 observations
      if (is.data.frame(current_observation) &&
          nrow(current_observation) >= 1500)
      {
        # Please note that these individual data.frames are not large enough to justify the usage of packages that optimize RAM usage
        # only keep relevant variables
        temp <-
          data.frame(
            date = as.Date(current_observation$valid),
            tmpf = current_observation$tmpf,
            relh = current_observation$relh,
            sknt = current_observation$sknt,
            p01i = current_observation$p01i
          )
        
        # aggregate observations over one day
        obsByDay <-
          group_by(temp, date) %>% summarize(
            tmpf = mean(tmpf, na.rm = TRUE),
            relh = mean(relh, na.rm = TRUE),
            sknt = mean(sknt, na.rm = TRUE),
            p01i = sum(p01i, na.rm = TRUE),
            station = first(station),
            lon = first(lon),
            lat = first(lat)
          )
        
        print(stat)
        print(count)
        if (count >= 10) {
          # currently limit to 10 weather stations per region
          break
        }
      }
    }
  }
  # write all observations of a given network to a .csv file
  write.csv(observations,
            paste("./data/weather/",
                  net,
                  ".csv"))
  observations <- data.frame()
}

#networks <- riem_networks()
networks <- networks[!networks$code %in% c("GF__ASOS", "KI__ASOS", "KP__ASOS", "MA__ASOS", "MG__ASOS", "YE__ASOS", "YT__ASOS"),] # removes empty networks

aobservations$ISO3 <-
  coordsToISO3(data.frame(
    lon = c(observations$lon),
    lat = c(observations$lat)
  )) # add country ISO3 code to every observation

observations <-
  observations[!is.na(observations$ISO3), ] # remove all observations where country is unknown


obs_cleaned <-
  observations %>% group_by(ISO3, date) %>% summarize(
    tmpf = mean(tmpf, na.rm = TRUE),
    relh = mean(relh, na.rm = TRUE),
    sknt = mean(sknt, na.rm = TRUE),
    p01i = mean(p01i, na.rm = TRUE),
    lon = first(lon),
    lat = first(lat)
  )


# We now add country ISO3 codes to each observation
wikipedia_iso_country_codes <-
  read_csv("./data/countries-iso-codes/wikipedia-iso-country-codes.csv")
iso_country_codes <- wikipedia_iso_country_codes[, (2:3)]
names(iso_country_codes) <-
  c("country", "ISO3") # create names for easier merge
obs <- merge(obs_cleaned, iso_country_codes)

# Write the collected data to database
bigdatadb <-
  dbConnect(
    RMariaDB::MariaDB(),
    user = 'bdauser',
    password = 'BDA4ever!!!',
    dbname = 'bigdatadb',
    host = '35.193.193.138'
  )
dbListTables(bigdatadb)

# create_table = "CREATE TABLE weather (ISO3 varchar(255), date Date, tmpf float, relh float, sknet float, p01i float, lon float, lat float);"
i = i
for (i in seq(3001, nrow(obs_cleaned), 1000)) {
  if (i + 1000 > nrow(obs_cleaned)) {
    dbWriteTable(bigdatadb,
                 "weather",
                 obs_cleaned[i:nrow(obs_cleaned),],
                 append = TRUE,
                 overwrite = FALSE)
  } else {
    dbWriteTable(
      bigdatadb,
      "weather",
      obs_cleaned[i:(i + 999),],
      append = TRUE,
      overwrite = FALSE,
      nrow = 50
    )
  }
  print(i)
}

# query = "SELECT * FROM openaq_location WHERE date <= '2020-04-30';"
openaq = dbGetQuery(bigdatadb, query)
# disconnect
dbDisconnect(bigdatadb)