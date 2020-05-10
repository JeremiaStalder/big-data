# librarys
library(tidyverse)
library(readr)
library(zoo)
library(RMariaDB) 
library(stats)
library(TSPred)
library(forecast)
library(lubridate)
library(data.table)

setwd("~/GitHub/big-data") # setwd
source("functions.R") # functions
outpath = "./output/detend_deseasonalize_airpollution_data/" # output

pollution_wheather_unified_units <- fread("data/pollution_wheather_unified_units.csv")
data_database = fread("data/merged.csv")

## simplified datacleaning 
pollution_test = pollution_wheather_unified_units[pollution_wheather_unified_units$value<=10^15,]
pollution_wheather_unified_units_means <- pollution_test[, .(mean = mean(value)), by=list(parameter, unit)]
pollution_wheather_unified_units_means

data_database_test = data_database[data_database$value<=10^15,]
data_database_means_daily <- data_database_test[, .(mean = mean(value)), by=list(parameter, unit)]
data_database_means_daily

data_database_means_counts <- data_database_test[, .N, by=list(parameter, unit)]
data_database_means_counts
