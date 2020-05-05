# Package for accessing Weather Data from the Iowa Environment Mesonet
# install.packages("riem")
# install.packages("sp")
# install.packages("rworldmap")
# install.packages("rworldxtra")

library(rworldxtra) # used for high resolution of world map
library(sp) # used for conversion from lat and lon to ISO3
library(rworldmap) # used for conversion from lat and lon to ISO3
library(riem)
library(dplyr)

# adapted from stack exchange 
coordsToISO3 = function(points)
{  
  countriesSP <- getMap(resolution='high')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  #indices$ADMIN  
  indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

# fetch all possible NOOA networks
networks <- riem_networks()

observations <- data.frame() # 
for (net in networks$code) {
  stations <- riem_stations(net)
  count <- 0
  for (stat in stations$id) {
    current_observation <- riem_measures(stat, date_start = "2016-01-01")
    if (is.data.frame(current_observation) && nrow(current_observation) >= 1500) {
      temp <-
        data.frame(
          date = as.Date(current_observation$valid),
          tmpf = current_observation$tmpf,
          relh = current_observation$relh,
          sknt = current_observation$sknt,
          p01i = current_observation$p01i
        )
      obsByDay <-
        group_by(temp, date) %>% summarize(
          tmpf = mean(tmpf, na.rm = TRUE),
          relh = mean(relh, na.rm = TRUE),
          sknt = mean(sknt, na.rm = TRUE),
          p01i = sum(p01i, na.rm = TRUE)
        )
      obsByDay$station = current_observation$station[1]
      obsByDay$lon = current_observation$lon[1]
      obsByDay$lat = current_observation$lat[1]
      observations <- rbind(observations, obsByDay)
      count <- count + 1
      print(stat)
      print(count)
      if (count >= 3) { # currently limit to 3 weather stations per region
        break
      }
    }
  }
  write.csv(
    observations,
    paste(
      "~/Desktop/master/big data/project/big-data/data/weather/",
      net,
      ".csv"
    )
  )
  observations <- data.frame()
}

networks <- riem_networks()
networks <- networks[-c(92, 128, 131, 149, 155, 263, 264), ]
observations <- data.frame()
# Todo: aggregate values for each region and then add it to one .csv
for (net in networks$code) {
  observations <-
    rbind(observations, read.csv(
      paste(
        "~/Desktop/master/big data/project/big-data/data/weather/",
        net,
        ".csv"
      )
    ))
}

observations$ISO3 <-
  coordsToISO3(data.frame(
    lon = c(observations$lon),
    lat = c(observations$lat)
  )) # add country ISO3 code to every observation

observations <- observations[!is.na(observations$ISO3), ] # remove all observations where country is unknown

obs_cleaned <-
  observations %>% group_by(ISO3, date) %>% summarize(
    tmpf = mean(tmpf, na.rm = TRUE),
    relh = mean(relh, na.rm = TRUE),
    sknt = mean(sknt, na.rm = TRUE),
    p01i = mean(p01i, na.rm = TRUE)
  )


# station: station name
# valid: timestamp of observation
# tmpf: Air temperature in Fahrenheit
# relh: Relative Humidity in %
# sknt: Wind Speed in knots
# p01i: precipitation

# See https://mesonet.agron.iastate.edu/request/download.phtml for more detailed documentatiion
