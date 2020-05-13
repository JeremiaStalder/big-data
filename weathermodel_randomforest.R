#install.packages("data.table")
#install.packages("randomForest")

#import packages
library(data.table)
library(randomForest)

# functions
impute.mean <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}

RMSE <- function(fitted, true){
  sqrt(mean((fitted - true)^2))
}

R2 <- function(fitted, true){
  1 - (sum((true - fitted)^2)/sum((true - mean(true))^2))
}


#set working diurectory
setwd("D:/Programming/R/BigDataAnalytics/data/weather")


#define variables
data_path = "data/pollution_wheather_unified_units.csv"


#load data from csv
data <- fread(data_path, verbose = TRUE, encoding = "UTF-8")

#drop irrelevant columns
data[, c("city", "station", "unit", "name", "gust", "visib", "dewp", "slp"):=NULL]

#ensure the data is unique
data = data[, .(value=mean(value), latitude=mean(latitude), longitude=mean(longitude), elevation=mean(elevation), temp=mean(temp), stp=mean(stp), wdsp=mean(wdsp), mxspd=mean(mxspd),  max=mean(max),  min=mean(min), prcp=mean(prcp), sndp=mean(sndp), rain=rain[1], snow = snow[1], hail= hail[1], thunder= thunder[1], tornado= tornado[1]), by=c("country", "state", "location", "parameter", "date")]

#create a month column
data[, c("day", "month", "year") := tstrsplit(date, "-")]
data[, c("day", "year"):=NULL]

#fill NA values
features = c("wdsp", "mxspd", "max", "min")
data[, (features) := lapply(.SD, impute.mean), by = state, .SDcols = features]
data[is.na(sndp), sndp := 0]

#drop rows with NA
data = data[complete.cases(data)]
cols = unique(data[, parameter])

#split the data by the different particle type
data = split(data, by="parameter")


#parameters
n_estimators = c(100, 200, 400, 800, 1600)

#empty df to store resutls
results <- matrix(0, nrow = length(seq_along(data)), ncol = length(n_estimators))

#fit models with different parameters
for (i in seq_along(data)){
  print(cols[i])
  
  #train-test splitting
  sample <- sample(1:nrow(data[[i]]), 0.7 * nrow(data[[i]]))
  gc()
  
  for (j in 1:length(n_estimators)){
    print(n_estimators[j])
    myrf = randomForest(data[[i]][sample, .(latitude, longitude, elevation, temp, stp, wdsp, mxspd, max, min, prcp, sndp, rain, snow, hail, thunder, tornado, month)], y=data[[i]][sample,value], n_tree=n_estimators[j])
    preds = predict(myrf, data[[i]][-sample, .(latitude, longitude, elevation, temp, stp, wdsp, mxspd, max, min, prcp, sndp, rain, snow, hail, thunder, tornado, month)])
    preds_r2 = R2(preds, data[[i]][-sample,value])
    results[i, j] = preds_r2
    print(preds_r2)
  }
}