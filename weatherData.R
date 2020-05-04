# Package for accessing Weather Data from the Iowa Environment Mesonet
# install.packages("riem")
library(riem)
library(dplyr)

# fetch all possible NOAA networks
networks <- riem_networks()

# networks <- networks$code[48:267]
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
  write.csv(observations, paste("~/Desktop/master/big data/project/big-data/data/weather/", net, ".csv"))
  observations <- data.frame()
}

# Todo: aggregate values for each region and then add it to one .csv  


    
# station: station name
# valid: timestamp of observation
# tmpf: Air temperature in Fahrenheit
# relh: Relative Humidity in %
# sknt: Wind Speed in knots
# p01i: precipitation

# See https://mesonet.agron.iastate.edu/request/download.phtml for more detailed documentatiion
