# detrend and deseasonalize air pollution data

# librarys
library(tidyverse)
library(readr)
library(zoo)
library(RMariaDB) 
library(stats)
library(TSPred)
library(forecast)

# functions
source("plotFunctions.R")

# output
outpath = outpath = "./output/detend_deseasonalize_airpollution_data/"

# import data----
  # connect to database 
  bigdatadb <- dbConnect(RMariaDB::MariaDB(), user='bdauser', password='BDA4ever!!!', dbname='bigdatadb', host='35.193.193.138')
  dbListTables(bigdatadb)
  
  # send query
  query = "SELECT * FROM openaq_location WHERE date <= '2020-04-30';"
  query = "SELECT * FROM openaq_locationWHERE date <= '2020-04-30';"
  openaq_state_original = dbGetQuery(bigdatadb,query)
  
#### DO DIFFERENT QUERY s.t. i get mean regions from location data
  # query = "SELECT AVG * FROM openaq_location WHERE date <= '2020-04-30';"
  # openaq_state_original_test = dbGetQuery(bigdatadb,query)
  
  # disconnect
  dbDisconnect(bigdatadb)
  
  # import mobililty data
  global_mobility_report_clean <- read_csv("data/clean/global_mobility_report_clean.csv", 
                                           col_types = cols(Date = col_date(format = "%Y-%m-%d")))
  global_mobility_report_clean = mutate(global_mobility_report_clean, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) # redo because "NA" is imported as NA
  global_mobility_report_clean = mutate(global_mobility_report_clean, sub_region_1 = ifelse(is.na(sub_region_1),CountryCode, sub_region_1)) # insert CountryCode as region to merge. Reason: if subregion in open aq missing, then country code is inserted. Is only inaccuarate in only a of the regions are reported
  
  # region name cleaning
  
  
# cleaning ----
  openaq_state = openaq_state_original
  
  # get same colnames as nationwide and mobility data for: date, countrycode, region
  key_variable_names = c("CountryCode","sub_region_1","Date")
  key_variable_names_openaq = c("country","state", "date")
  colnames(openaq_state)[colnames(openaq_state) %in% key_variable_names_openaq] = key_variable_names 
  
  summary(openaq_state)
  
  
  openaq_state_clean = openaq_state # finished cleaning
  
# merge data
  
  # test if same region names (key merging variable)
  
  length(unique(global_mobility_report_clean$sub_region_1))
  length(unique(openaq_state_clean$sub_region_1))
  indicator_change_region_name = sort(match(unique(global_mobility_report_clean$sub_region_1), unique(openaq_state_clean$sub_region_1))) # identify matched regions
  b = unique(openaq_state_clean$sub_region_1)[-indicator_change_region_name] # identify non-matched regions
  
  filter_mobility = unique(filter(global_mobility_report_clean, CountryCode =="DE" &  Date == "2020-02-20")$sub_region_1) 
  b
  filter_mobility
  
  # merge by CountryCode, sub_region_1 and Date (CountryCode should not be necessary if no mistakes before)
  merged_data = inner_join(global_mobility_report_clean, openaq_state_clean, by = c("CountryCode", "sub_region_1", "Date")) 
  # summary(merged_data)
  # summary(is.na(merged_data))
  
  regions = unique(merged_data$sub_region_1)
  
  
# Plot climate data
  
  # get one region: 
  region = "Abu Dhabi"
  region = "Salzburg"
  # region = "Bogota"
  # region = "Saxony-Anhalt"

  data_plot = filter(merged_data, sub_region_1 == region)
  data_parameters = unique(data_plot$parameter)
  data_parameters
  
  
  y1 = filter(data_plot, parameter == unique(data_plot$parameter)[1])
  y2 = filter(data_plot, parameter == unique(data_plot$parameter)[2])
  y3 = filter(data_plot, parameter == unique(data_plot$parameter)[3])
  y4 = filter(data_plot, parameter == unique(data_plot$parameter)[4])
  y5 = filter(data_plot, parameter == unique(data_plot$parameter)[5])
  
  y1$value
  y2$value
  
  line_plot_multiple(paste("Climate Data - ", region), outpath,y1$Date,"Date", "Airpolltion", names_y=unique(data_plot$parameter), 
                     y_percent=F, legend=T,y1$value, y2$value*50, y3$value*4, y4$value*10, y5$value*100)
  
# test decompose
  
  # produce test time series object
    drift = 1
    sigma = 5
    number_obs = 50
    
    test_data = ts(sigma * sin(c(1:number_obs)*pi*0.5+0.5*pi) + drift *seq(number_obs), start =1, end = number_obs, frequency = number_obs)
    test_data_diff = test_data - stats::lag(test_data)
    test_data_diff
    
    pacf(test_data, lag.max = 60)
    
    names_y = c("true","trend", "seasonal", "error")
    
    decomposed_data <- decompose(test_data)
    
    y1_ts = as.vector(decomposed_data$x)
    y2_ts = as.vector(decomposed_data$trend)
    y3_ts = as.vector(decomposed_data$seasonal)
    y4_ts =as.vector(decomposed_data$random)
    
    line_plot_multiple(paste("Decomposed TS - ", region), outpath,seq(1:length(y1_ts)),"Date", "Airpolltion", names_y=names_y, 
                       y_percent=F, legend=T,y1_ts, y2_ts,y3_ts,y4_ts)
    
    
    # Decompose on real data
    
    test_data = ts(y1$value, start =1, end = length(y1$value), frequency = length(y1$value))
    test_data
    
    
    pacf(test_data, lag.max = 50)
    acf(test_data, lag.max = 50) # 
    
    names_y = c("true","trend", "seasonal", "error")
    
    decomposed_data <- decompose(test_data,type = ("multiplicative"))
    
    y1_ts = as.vector(decomposed_data$x)
    y2_ts = as.vector(decomposed_data$trend)
    y3_ts = as.vector(decomposed_data$seasonal)
    y4_ts =as.vector(decomposed_data$random)
    
    line_plot_multiple(paste("Decomposed TS - ", region), outpath,seq(1:length(y1_ts)),"Date", "Airpolltion", names_y=names_y, 
                       y_percent=F, legend=T,y1_ts, y2_ts,y3_ts,y4_ts)
    
  # Try FILTER
    
    test_data = ts(y1$value, start =1, end = length(y1$value), frequency = 12)
    test_data
    
    arima = auto.arima(y = test_data, seasonal=F, ic="bic")
    arima
    
    arima_2 = arima(test_data, c(0,1,0))
    
    test_data_arima =  forecast(arima, h=10)
    forecast(arima_2, h=10)
    
    pacf(test_data, lag.max = 50)
    pacf(test_data_arima$residuals, lag.max = 50)
    
    names_y = c("true","trend", "seasonal", "error")
    
    y1_ts = as.vector(decomposed_data$x)
    y2_ts = as.vector(decomposed_data$trend)
    y3_ts = as.vector(decomposed_data$seasonal)
    y4_ts =as.vector(decomposed_data$random)
    
    line_plot_multiple(paste("Decomposed TS - ", region), outpath,seq(1:length(y1_ts)),"Date", "Airpolltion", names_y=names_y, 
                       y_percent=F, legend=T,y1_ts, y2_ts,y3_ts,y4_ts)
  
  
  
  
  
  
  
  
  
  
  
  
  