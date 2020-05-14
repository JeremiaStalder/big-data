# Merge Airpollution Data, Predicted Airpollution and Stringency Index Data

# librarys
  library(readr)
  library(zoo)
  library(RMariaDB) 
  library(stats)
  library(TSPred)
  library(forecast)
  library(lubridate)
  library(data.table)
  library(tidyverse)
  
  filter <- dplyr::filter
  select <- dplyr::select
  
  setwd("~/GitHub/big-data") # setwd
  source("functions.R") # functions
  outpath = "./output/detend_deseasonalize_airpollution_data/" # output
  outpath_files = "./data/output_openaqData/" # output

# Import data----

  # airpollution data
    # load data from database (and save file from database) or locally (access saved file from database)
    # local file implemented to prevent waiting times from database
    load_locally = 1
    unit_conversion_openaq = F # do not convert units if inputs data already did

    if(load_locally ==1) {
      openaq_state_original = read_csv("./data/clean/openaq_state_original.csv", 
                                       col_types = cols(date = col_date(format = "%Y-%m-%d"))) 
      colnames(openaq_state_original)[colnames(openaq_state_original) == "AVG.value."] = "value"
    } else {
    # connect to database 
    bigdatadb <- dbConnect(RMariaDB::MariaDB(), user='bdauser', password='BDA4ever!!!', dbname='bigdatadb', host='35.193.193.138')
    dbListTables(bigdatadb)
    
    # send query
    query = "SELECT country, state, date, parameter, unit , AVG(value) FROM bigdatadb.openaq WHERE date between '2018-01-01' AND '2020-04-30' GROUP BY country, state, date, parameter, unit;"
    openaq_state_original = dbGetQuery(bigdatadb,query)
    
    # disconnect
    dbDisconnect(bigdatadb)
    
    # save as csv
    write.csv(openaq_state_original, "./data/clean/openaq_state_original.csv")
    }
    
    
  # unit conversion data for airpollution
     unit_conversion_table <- read_delim("./data/unit_conversion/unit_conversion.csv", 
                                                            ";", escape_double = FALSE, col_types = cols(value = col_double()), 
                                                            locale = locale(decimal_mark = ","), 
                                                            trim_ws = TRUE)

  # mobility data
    # import mobililty data
    global_mobility_report_clean_stringency_index <- read_csv("data/clean/global_mobility_report_clean_with_predictions_stringency_index.csv", 
                                                              col_types = cols(CountryName = col_character(), 
                                                                                     Date = col_date(format = "%Y-%m-%d"), 
                                                                                     CountryCode = col_character(), 
                                                                                     sub_region_1 = col_character(),
                                                                                     grocery_and_pharmacy = col_double(), 
                                                                                     parks = col_double(), residential = col_double(), 
                                                                                     retail_and_recreation = col_double(), 
                                                                                     sub_region_1 = col_character(), transit_stations = col_double(), 
                                                                                     workplaces = col_double()))
    
    global_mobility_report_clean_stringency_index = mutate(global_mobility_report_clean_stringency_index, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) # redo because "NA" is imported as NA
    global_mobility_report_clean_stringency_index = mutate(global_mobility_report_clean_stringency_index, sub_region_1 = ifelse(is.na(sub_region_1),CountryCode, sub_region_1)) # insert CountryCode as region to merge. Reason: if subregion in open aq missing, then country code is inserted. Is only inaccuarate in only a of the regions are reported
    
    # change to lowercase. encoding latin1 inbetween to prevent errror
      Encoding(global_mobility_report_clean_stringency_index$sub_region_1) = "latin1"
      Encoding(global_mobility_report_clean_stringency_index$CountryCode) = "latin1"
      Encoding(global_mobility_report_clean_stringency_index$CountryName) = "latin1"
      global_mobility_report_clean_stringency_index$sub_region_1 = tolower(global_mobility_report_clean_stringency_index$sub_region_1)
      global_mobility_report_clean_stringency_index$CountryCode = tolower(global_mobility_report_clean_stringency_index$CountryCode)
      global_mobility_report_clean_stringency_index$CountryName = tolower(global_mobility_report_clean_stringency_index$CountryName)
      Encoding(global_mobility_report_clean_stringency_index$sub_region_1) = "UTF-8"
      Encoding(global_mobility_report_clean_stringency_index$CountryCode) = "UTF-8"
      Encoding(global_mobility_report_clean_stringency_index$CountryName) = "UTF-8"


  # import predictions for airpollution based on weather data
    # no predictions for bc due to low number of observations and locations
    co_prediction <- read_csv("data/predictionAirpollutionFromWeatherData/co_prediction.csv")
    no2_prediction <- read_csv("data/predictionAirpollutionFromWeatherData/no2_prediction.csv")
    o3_prediction <- read_csv("data/predictionAirpollutionFromWeatherData/o3_prediction.csv")
    pm10_prediction <- read_csv("data/predictionAirpollutionFromWeatherData/pm10_prediction.csv")
    pm25_prediction <- read_csv("data/predictionAirpollutionFromWeatherData/pm25_prediction.csv")
    so2_prediction <- read_csv("data/predictionAirpollutionFromWeatherData/so2_prediction.csv")
    
    # add parameter as variable
    co_prediction$parameter = "co"
    no2_prediction$parameter = "no3"
    o3_prediction$parameter = "o3"
    pm10_prediction$parameter = "pm10"
    pm25_prediction$parameter = "pm25"
    so2_prediction$parameter = "so2"
    
    predicted_airpollution = rbind(co_prediction, no2_prediction, o3_prediction, pm10_prediction, pm25_prediction, so2_prediction)
    colnames(predicted_airpollution)[colnames(predicted_airpollution) %in% c("state")] = "sub_region_1"
    colnames(predicted_airpollution)[colnames(predicted_airpollution) %in% c("date")] = "Date"
    
    # remove negative predictions if any
    predicted_airpollution$prediction[(predicted_airpollution$prediction<0)]= 0 

# Cleaning OpenAQ----
  openaq_state = openaq_state_original
  
  # drop non-predicted particle bc
    openaq_state = filter(openaq_state, parameter != "bc")
    
  # get same colnames as nationwide and mobility data for: date, countrycode, region
    key_variable_names = c("CountryCode","sub_region_1","Date")
    key_variable_names_openaq = c("country","state", "date")
    colnames(openaq_state)[colnames(openaq_state) %in% key_variable_names_openaq] = key_variable_names 
    
  # lowercase country code
    openaq_state$CountryCode = tolower(openaq_state$CountryCode)
  
  # rename value col
    colnames(openaq_state)[colnames(openaq_state) == "AVG(value)"] = "value"
    summary(openaq_state)
    
  # convert units from ppm to microgram/m3 if not already done input data
    if (unit_conversion_openaq==T) {
      # inputs for function
      list_particles_convert = unique(select(filter(openaq_state, unit == "ppm"), parameter))$parameter
      
      particle  = openaq_state$parameter[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ]
      tempF = rep(NA, length(particle))
      pressure_millibars_to_tenth = rep(NA, length(particle))
      concentration_pmm = openaq_state$value[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ]
      # call function
      openaq_state$value[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ] = ppm_to_microgram(particle,tempF, pressure_millibars_to_tenth, concentration_pmm, unit_conversion_table)
      openaq_state$unit[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ] = "microgram_per_m3_transformed"
    }

    # extreme outliers: replace wrt quantile
    for (i in 1:length(unique(openaq_state$parameter))) {
      openaq_state[(openaq_state$parameter == unique(openaq_state$parameter)[i]),"value"] = remove_outliers(filter(openaq_state, parameter == unique(openaq_state$parameter)[i])$value)
    }
      
    # check summary stats for units after outlier cleaning
    unit_compare = group_by(openaq_state, parameter, unit) %>%
      summarize(mean = mean(value))
    unit_compare
  
  # variables for analysis
  airquality_variables = c("parameter", "value")
  stringency_variables = c("StringencyIndex","retail_and_recreation","grocery_and_pharmacy","parks","transit_stations","workplaces","residential")
  category_variables = c("CountryName","Region")
  cross_section_variables = c("ConfirmedCases","Population","Pop. Density (per sq. mi.)","GDP ($ per capita)","Agriculture","Industry","Service")
  
  analysis_variables = c(airquality_variables, stringency_variables, category_variables, cross_section_variables)
  
  open_state_clean = openaq_state # use raw data, take rolling averages later. !! CAREFUL !! open_state_clean will be merged with predictions, edited and get same name later
  
  
# Merge Covid & Airpollution & Prediction: ----
  #  merge predicted airpollution to airpollution data
  # only analyze airpollution regions that are reported in other datasets.
  # change airpollution dataset for descriptives
  # merge again before effect calculation (only limited timeframe)
  
  # Merge prediction and airpollution data
    # test # of matching regions
      indicator_change_region_name = sort(match(unique(predicted_airpollution$sub_region_1), unique(open_state_clean$sub_region_1))) # identify matched regions
      not_matched_regions = unique(open_state_clean$sub_region_1)[-indicator_change_region_name] # identify non-matched regions
      print(paste("Merging Predictions from Weather Data & Airpollution:",length(not_matched_regions), "Subregions are not matched. Prediction will be NA for not-matched regions"))
    # merge
      merged_openaq_prediciton = right_join(predicted_airpollution, open_state_clean, by = c("sub_region_1", "Date", "parameter")) 
    
  # Covid & Airpollution
    # replace region with country in openaq if no match in mobility data
    indicator_change_region_name = sort(match(unique(global_mobility_report_clean_stringency_index$sub_region_1), unique(merged_openaq_prediciton$sub_region_1))) # identify matched regions
    not_matched_regions = unique(merged_openaq_prediciton$sub_region_1)[-indicator_change_region_name] # identify non-matched regions
    merged_openaq_prediciton[(merged_openaq_prediciton$sub_region_1 %in% not_matched_regions),]$sub_region_1 = merged_openaq_prediciton[(merged_openaq_prediciton$sub_region_1 %in% not_matched_regions),]$CountryCode
    
    # test match after replacing
    indicator_change_region_name_1 = sort(match(unique(global_mobility_report_clean_stringency_index$sub_region_1), unique(merged_openaq_prediciton$sub_region_1))) 
    not_matched_regions_1 = unique(merged_openaq_prediciton$sub_region_1)[-indicator_change_region_name_1]
    print(paste("Merging Stringency & Airpollution:",length(not_matched_regions_1), "Subregions are not matched"))
    
   # test merge by CountryCode, sub_region_1 and Date (CountryCode should not be necessary if no mistakes before)
    # dataset will not be saved, we merge data again when necessary so only relevant variables are in dataframe
    merged_data = inner_join(global_mobility_report_clean_stringency_index, merged_openaq_prediciton, by = c("CountryCode", "sub_region_1", "Date")) 
    summary(merged_data)
  
# Airpollution Data Clean Step 2 ---- 
    # add regions to entire open_state_clean dataset
    list_country_code_region = select(global_mobility_report_clean_stringency_index, sub_region_1, Region) %>%
      distinct()
    open_state_clean = left_join(merged_openaq_prediciton, list_country_code_region, by=("sub_region_1"))
    
    # Average over new subregions after merging 
    open_state_clean = group_by(open_state_clean, Region, CountryCode, Date, sub_region_1, parameter, unit) %>%
      summarize(value = mean(value, na.rm=T), prediction = mean(prediction, na.rm=T)) %>%
      ungroup()
    
    # Create Last years value as variable
    open_state_clean_previous_year = mutate(open_state_clean, Date = Date+years(1))  %>%
      rename(value_last_year = value)# shift date 1 year ahead to merge
    
    open_state_clean = left_join(open_state_clean, select(open_state_clean_previous_year, Date, CountryCode, sub_region_1, parameter,value_last_year), by = c("Date", "CountryCode", "sub_region_1", "parameter"))
    open_state_clean$value_difference = open_state_clean$value- open_state_clean$value_last_year
    open_state_clean$error_prediction = open_state_clean$value - open_state_clean$prediction
    
  # Create two additional versions of dataset   
    # Create Rolling Average Dataset
      
      # rolling average for value to figure out seasonal patterns. 
      parms_ma = list(15, 30, 60, 8, 15, 30)
      names(parms_ma) = c("short","medium","long","max_na_short","max_na_medium","max_na_long")
      
      openaq_state_ma = group_by(open_state_clean, CountryCode,sub_region_1, parameter, unit) %>%
        arrange(Date) %>%
        mutate(value_indicator = ifelse(is.na(value), 1, 0)) %>% # to "sum" NAs is condition for when rolling is calculated
        mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$short,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
        mutate(prediction_indicator = ifelse(is.na(value), 1, 0)) %>% # to "sum" NAs is condition for when rolling is calculated
        mutate(prediction = ifelse((rollapply(prediction_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(prediction,parms_ma$short,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
        mutate(error_prediction = value- prediction ) %>%
        ungroup()# get monthly moving average if more than share of the values are provided
      
      openaq_state_clean_ma = openaq_state_ma # rolling average data
      summary(openaq_state_clean_ma)
      
  # country difference
  country_difference_data = group_by(open_state_clean, parameter, Date, Region,CountryCode) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T), prediction = mean(prediction, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    ungroup(Date) %>%
    arrange(Date) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year) %>%
    mutate(prediction_indicator = ifelse(is.na(prediction), 1, 0)) %>% # to "sum" NAs is condition for when rolling is calculated
    mutate(prediction = ifelse((rollapply(prediction_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(prediction,parms_ma$short,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(error_prediction =value - prediction) %>%
    ungroup(Region,CountryCode)
  
  # standardize (value-mean)/sd
  country_difference_data_standardized = group_by(country_difference_data, parameter, Region,CountryCode) %>%
    mutate(value = (value-mean(value,na.rm=T))/sd(value, na.rm=T), value_last_year = (value_last_year-mean(value_last_year,na.rm=T))/sd(value_last_year, na.rm=T)) %>%
    mutate( value_difference = value - value_last_year)%>%
    mutate(prediction = (prediction-mean(prediction,na.rm=T))/sd(prediction, na.rm=T)) %>%
    mutate(error_prediction =value - prediction) %>%
    ungroup(Region,CountryCode)
  
  # subregion difference
  subregion_difference_data = group_by(open_state_clean, parameter, Date, Region,CountryCode, sub_region_1) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T), prediction = mean(prediction, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    ungroup(Date) %>%
    arrange(Date) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year) %>%
    mutate(prediction_indicator = ifelse(is.na(prediction), 1, 0)) %>% # to "sum" NAs is condition for when rolling is calculated
    mutate(prediction = ifelse((rollapply(prediction_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(prediction,parms_ma$short,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(error_prediction = value- prediction) %>%
    ungroup(Region,CountryCode)
  
  
  # standardize (value-mean)/sd. Edit: should only take mean up to covid time 
  subregion_difference_data_standardized = group_by(subregion_difference_data, parameter, Region,CountryCode, sub_region_1) %>%
    mutate(value = (value-mean(value,na.rm=T))/sd(value, na.rm=T), value_last_year = (value_last_year-mean(value_last_year,na.rm=T))/sd(value_last_year, na.rm=T)) %>%
    mutate( value_difference = value - value_last_year) %>%
    mutate(prediction = (prediction-mean(prediction,na.rm=T))/sd(prediction, na.rm=T)) %>%
    mutate(error_prediction = value- prediction) %>%
    ungroup(Region,CountryCode)
  
  # create list of countries and subregions that exist in both datasets
    countryListOpenaqStringencyMerged= select(global_mobility_report_clean_stringency_index, sub_region_1, CountryCode, CountryName, Region) %>%
      distinct()
  
  # dataset used for presentation: includes all data for relevant timeframe
    # merge google and subregion airpollution data again
    all_data_over_time = inner_join(global_mobility_report_clean_stringency_index, select(subregion_difference_data, -c("CountryCode", "Region")), by = c("sub_region_1","Date")) 
    
    # drop unnecessary vars
    all_data_over_time = select(all_data_over_time, -c("value_last_year","prediction_indicator","value_last_year_indicator","X1","X1_1", "LegacyStringencyIndex","LegacyStringencyIndexForDisplay")) %>%
      filter(Date >='2020-02-15' & Date <='2020-04-20')
    
  # save data
  write.csv(open_state_clean,file=paste0(outpath_files,"open_state_clean.csv"))
  write.csv(openaq_state_clean_ma,file=paste0(outpath_files,"openaq_state_clean_ma.csv"))
  write.csv(country_difference_data,file=paste0(outpath_files,"country_difference_data.csv"))
  write.csv(country_difference_data_standardized,file=paste0(outpath_files,"country_difference_data_standardized.csv"))
  write.csv(subregion_difference_data,file=paste0(outpath_files,"subregion_difference_data.csv"))
  write.csv(subregion_difference_data_standardized,file=paste0(outpath_files,"subregion_difference_data_standardized.csv"))
  write.csv(global_mobility_report_clean_stringency_index,file=paste0(outpath_files,"global_mobility_report_clean_to_merge_with_openaq.csv"))
  write.csv(countryListOpenaqStringencyMerged,file=paste0(outpath_files,"countryListOpenaqStringencyMerged.csv"))
  write.csv(all_data_over_time,file=paste0(outpath_files,"all_data_over_time.csv"))
  
  
  
