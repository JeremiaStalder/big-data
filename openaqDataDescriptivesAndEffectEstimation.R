# detrend and deseasonalize air pollution data

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
  

# Import data----

  # airpollution data
    # load data from locally or from database (save file from database).
    # Note: local file implemented to prevent waiting times from database
    load_locally = 1
    unit_conversion_openaq = T # do not convert units if inputs data already did

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
                                                                                     grocery_and_pharmacy = col_double(), 
                                                                                     parks = col_double(), residential = col_double(), 
                                                                                     retail_and_recreation = col_double(), 
                                                                                     sub_region_1 = col_character(), transit_stations = col_double(), 
                                                                                     workplaces = col_double()))
    
    global_mobility_report_clean_stringency_index = mutate(global_mobility_report_clean_stringency_index, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) # redo because "NA" is imported as NA
    global_mobility_report_clean_stringency_index = mutate(global_mobility_report_clean_stringency_index, sub_region_1 = ifelse(is.na(sub_region_1),CountryCode, sub_region_1)) # insert CountryCode as region to merge. Reason: if subregion in open aq missing, then country code is inserted. Is only inaccuarate in only a of the regions are reported
    global_mobility_report_clean_stringency_index$sub_region_1 = tolower(global_mobility_report_clean_stringency_index$sub_region_1)
    global_mobility_report_clean_stringency_index$CountryCode = tolower(global_mobility_report_clean_stringency_index$CountryCode)
    global_mobility_report_clean_stringency_index$CountryName = tolower(global_mobility_report_clean_stringency_index$CountryName)

# Cleaning OpenAQ----
  openaq_state = openaq_state_original
  
    
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
  
  open_state_clean = openaq_state # use raw data, take rolling averages later
  
# Merge Covid & Airpollution: ----
  # Aim: only analyze airpollution regions that are in covid data.
  # change airpollution dataset for descriptives
  # merge again before effect calculation (only limited timeframe)
  
  
    # replace region with country in openaq if no match in mobility data
    indicator_change_region_name = sort(match(unique(global_mobility_report_clean_stringency_index$sub_region_1), unique(open_state_clean$sub_region_1))) # identify matched regions
    not_matched_regions = unique(open_state_clean$sub_region_1)[-indicator_change_region_name] # identify non-matched regions
    open_state_clean[(open_state_clean$sub_region_1 %in% not_matched_regions),]$sub_region_1 = open_state_clean[(open_state_clean$sub_region_1 %in% not_matched_regions),]$CountryCode
    
    # test match
    indicator_change_region_name_1 = sort(match(unique(global_mobility_report_clean_stringency_index$sub_region_1), unique(open_state_clean$sub_region_1))) 
    not_matched_regions_1 = unique(open_state_clean$sub_region_1)[-indicator_change_region_name_1]
    print(paste("Merging:",length(not_matched_regions_1), "Subregions are not matched"))
    
    # merge by CountryCode, sub_region_1 and Date (CountryCode should not be necessary if no mistakes before)
    merged_data = inner_join(global_mobility_report_clean_stringency_index, open_state_clean, by = c("CountryCode", "sub_region_1", "Date")) 
    summary(merged_data)
    
    regions = unique(merged_data$sub_region_1)  
    
  
# Airpollution Data Clean Step 2 ---- 
    # add regions to entire open_state_clean dataset
    list_country_code_region = select(global_mobility_report_clean_stringency_index, sub_region_1, Region) %>%
      distinct()
    open_state_clean = left_join(open_state_clean, list_country_code_region, by=("sub_region_1"))
    
    # Average over new subregions after merging
    open_state_clean = group_by(open_state_clean, Region, CountryCode, Date, sub_region_1, parameter, unit) %>%
      summarize(value = mean(value, na.rm=T)) %>%
      ungroup()
    
    # Create Last years value as variable
    open_state_clean_previous_year = mutate(open_state_clean, Date = Date+years(1))  %>%
      rename(value_last_year = value)# shift date 1 year ahead to merge
    
    open_state_clean = left_join(open_state_clean, select(open_state_clean_previous_year, Date, CountryCode, sub_region_1, parameter,value_last_year), by = c("Date", "CountryCode", "sub_region_1", "parameter"))
    open_state_clean$value_difference = open_state_clean$value- open_state_clean$value_last_year
    
  # Create two additional versions of dataset   
    # Create Rolling Average Dataset
      
      # rolling average for value to figure out seasonal patterns. 
      parms_ma = list(15, 30, 60, 8, 15, 30)
      names(parms_ma) = c("short","medium","long","max_na_short","max_na_medium","max_na_long")
      
      openaq_state_ma = group_by(open_state_clean, CountryCode,sub_region_1, parameter, unit) %>%
        arrange(Date) %>%
        mutate(value_indicator = ifelse(is.na(value), 1, 0)) %>% # to "sum" NAs is condition for when rolling is calculated
        mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$short,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
        ungroup()# get monthly moving average if more than share of the values are provided
      
      openaq_state_clean_ma = openaq_state_ma # rolling average data
      
  # country difference
  country_difference_data = group_by(open_state_clean, parameter, Date, Region,CountryCode) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    ungroup(Date) %>%
    arrange(Date) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year)
  
  # standardize (value-mean)/sd
  country_difference_data_standardized = group_by(country_difference_data, parameter, Region,CountryCode) %>%
    mutate(value = (value-mean(value,na.rm=T))/sd(value, na.rm=T), value_last_year = (value_last_year-mean(value_last_year,na.rm=T))/sd(value_last_year, na.rm=T)) %>%
    mutate( value_difference = value - value_last_year)
  
  # subregion difference
  subregion_difference_data = group_by(open_state_clean, parameter, Date, Region,CountryCode, sub_region_1) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    ungroup(Date) %>%
    arrange(Date) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year)
  
  # standardize (value-mean)/sd. Edit: should only take mean up to covid time 
  subregion_difference_data_standardized = group_by(subregion_difference_data, parameter, Region,CountryCode, sub_region_1) %>%
    mutate(value = (value-mean(value,na.rm=T))/sd(value, na.rm=T), value_last_year = (value_last_year-mean(value_last_year,na.rm=T))/sd(value_last_year, na.rm=T)) %>%
    mutate( value_difference = value - value_last_year)
    
    
# Descriptive Airpollution Raw Data ----   
 
  # Plot pollution data
    
  # worldwide
  data_wide = group_by(open_state_clean, parameter, Date) %>%
      summarize(value = mean(value, na.rm=T)) %>%
      mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
   mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
  pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = value) %>%
    ungroup()
  
  # add 0s for non-measures parms
  data_wide = mutate(data_wide, bc = ifelse(rep("bc", nrow(data_wide)) %in% colnames(data_wide), bc, rep(0, nrow(data_wide))), 
                     co = ifelse(rep("co", nrow(data_wide)) %in% colnames(data_wide), co, rep(0, nrow(data_wide))), 
                     no2 = ifelse(rep("no2", nrow(data_wide)) %in% colnames(data_wide), no2, rep(0, nrow(data_wide))),
                     o3 = ifelse(rep("o3", nrow(data_wide)) %in% colnames(data_wide), o3, rep(0, nrow(data_wide))),
                     pm10 = ifelse(rep("pm10", nrow(data_wide)) %in% colnames(data_wide), pm10, rep(0, nrow(data_wide))),
                     pm25 = ifelse(rep("pm25", nrow(data_wide)) %in% colnames(data_wide), pm25, rep(0, nrow(data_wide))),
                     so2 = ifelse(rep("so2", nrow(data_wide)) %in% colnames(data_wide), so2, rep(0, nrow(data_wide))))
  
  print(line_plot_multiple(paste("Airpollution Data - World"), outpath,data_wide$Date,"Date", "Airpollution", names_y=unique(open_state_clean$parameter), 
                           y_percent=F, legend=T, data_wide$co / mean(data_wide$co, na.rm=T), data_wide$no2 / mean(data_wide$no2, na.rm=T), data_wide$o3 / mean(data_wide$o3, na.rm=T), 
                           data_wide$pm10 / mean(data_wide$pm10, na.rm=T), data_wide$pm25 / mean(data_wide$pm25, na.rm=T), data_wide$so2 / mean(data_wide$so2, na.rm=T), data_wide$bc / mean(data_wide$bc, na.rm=T)))
  
  
  # regions
  data_plot = group_by(open_state_clean, parameter, Date, Region) %>%
    summarize(value = mean(value, na.rm=T)) %>%
    group_by(parameter,Region) %>%
    arrange(Date) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA))
  
  for (i in 1:length(unique(data_plot$Region))) {
    if(is.na(unique(data_plot$Region)[i])==F) {
    data_wide = filter(data_plot, Region ==unique(data_plot$Region)[i]) %>%
      pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = value) %>%
      ungroup()

    # add 0s for non-measures parms
    data_wide = mutate(data_wide, bc = ifelse(rep("bc", nrow(data_wide)) %in% colnames(data_wide), bc, rep(0, nrow(data_wide))), 
                       co = ifelse(rep("co", nrow(data_wide)) %in% colnames(data_wide), co, rep(0, nrow(data_wide))), 
                       no2 = ifelse(rep("no2", nrow(data_wide)) %in% colnames(data_wide), no2, rep(0, nrow(data_wide))),
                       o3 = ifelse(rep("o3", nrow(data_wide)) %in% colnames(data_wide), o3, rep(0, nrow(data_wide))),
                       pm10 = ifelse(rep("pm10", nrow(data_wide)) %in% colnames(data_wide), pm10, rep(0, nrow(data_wide))),
                       pm25 = ifelse(rep("pm25", nrow(data_wide)) %in% colnames(data_wide), pm25, rep(0, nrow(data_wide))),
                       so2 = ifelse(rep("so2", nrow(data_wide)) %in% colnames(data_wide), so2, rep(0, nrow(data_wide))))
    
    print(line_plot_multiple(paste("Airpollution Data -", unique(data_plot$Region)[i]), outpath,data_wide$Date,"Date", "Airpollution", names_y=unique(data_plot$parameter), 
                             y_percent=F, legend=T, data_wide$co / mean(data_wide$co, na.rm=T), data_wide$no2 / mean(data_wide$no2, na.rm=T), data_wide$o3 / mean(data_wide$o3, na.rm=T), 
                             data_wide$pm10 / mean(data_wide$pm10, na.rm=T), data_wide$pm25 / mean(data_wide$pm25, na.rm=T), data_wide$so2 / mean(data_wide$so2, na.rm=T), data_wide$bc / mean(data_wide$bc, na.rm=T)))
    }
  }

  # countries
  
  list_countries = c("us", "de", "it", "cn","af","ae", "au")
  
  data_plot = filter(open_state_clean, CountryCode %in% list_countries) %>%
    group_by(parameter, Date, CountryCode) %>%
    summarize(value = mean(value, na.rm=T)) %>%
    group_by(parameter,CountryCode) %>%
    arrange(Date) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA))
  
  
  for (i in 1:length(unique(data_plot$CountryCode))) {
    data_wide = filter(data_plot, CountryCode ==unique(data_plot$CountryCode)[i]) %>%
      pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = value) %>%
      ungroup()
    
    # add 0s for non-measures parms
    data_wide = mutate(data_wide, bc = ifelse(rep("bc", nrow(data_wide)) %in% colnames(data_wide), bc, rep(0, nrow(data_wide))), 
                       co = ifelse(rep("co", nrow(data_wide)) %in% colnames(data_wide), co, rep(0, nrow(data_wide))), 
                       no2 = ifelse(rep("no2", nrow(data_wide)) %in% colnames(data_wide), no2, rep(0, nrow(data_wide))),
                       o3 = ifelse(rep("o3", nrow(data_wide)) %in% colnames(data_wide), o3, rep(0, nrow(data_wide))),
                       pm10 = ifelse(rep("pm10", nrow(data_wide)) %in% colnames(data_wide), pm10, rep(0, nrow(data_wide))),
                       pm25 = ifelse(rep("pm25", nrow(data_wide)) %in% colnames(data_wide), pm25, rep(0, nrow(data_wide))),
                       so2 = ifelse(rep("so2", nrow(data_wide)) %in% colnames(data_wide), so2, rep(0, nrow(data_wide))))
    
    print(line_plot_multiple(paste("Airpollution Data -", unique(data_plot$CountryCode)[i]), outpath,data_wide$Date,"Date", "Airpollution", names_y=unique(data_plot$parameter), 
                             y_percent=F, legend=T, data_wide$co / mean(data_wide$co, na.rm=T), data_wide$no2 / mean(data_wide$no2, na.rm=T), data_wide$o3 / mean(data_wide$o3, na.rm=T), 
                             data_wide$pm10 / mean(data_wide$pm10, na.rm=T), data_wide$pm25 / mean(data_wide$pm25, na.rm=T), data_wide$so2 / mean(data_wide$so2, na.rm=T), data_wide$bc / mean(data_wide$bc, na.rm=T)))
  }
  
  # subregions
  
  list_countries = c("us", "de", "it", "cn","af","ae", "au","ca", "ch","fr", "es")
  list_subregions = c("mecklenburg-vorpommern","washington","colorado","shandong","beijing")
  list_subregions = c("wuhan","bremen","hamburg","berlin","brandenburg")
  
  data_plot = filter(open_state_clean, (CountryCode %in% list_countries) & (sub_region_1 %in% list_subregions)) %>%
    group_by(parameter, Date, CountryCode, sub_region_1) %>%
    summarize(value = mean(value, na.rm=T)) %>%
    group_by(parameter,sub_region_1) %>%
    arrange(Date) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA))
  
  
  for (i in 1:length(unique(data_plot$sub_region_1))) {
    data_wide = filter(data_plot, sub_region_1 ==unique(data_plot$sub_region_1)[i]) %>%
      pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = value) %>%
      ungroup()
    
  # add 0s for non-measures parms
  data_wide = mutate(data_wide, bc = ifelse(rep("bc", nrow(data_wide)) %in% colnames(data_wide), data_wide$bc, rep(0, nrow(data_wide))), 
                     co = ifelse(rep("co", nrow(data_wide)) %in% colnames(data_wide), co, rep(0, nrow(data_wide))), 
                     no2 = ifelse(rep("no2", nrow(data_wide)) %in% colnames(data_wide), no2, rep(0, nrow(data_wide))),
                     o3 = ifelse(rep("o3", nrow(data_wide)) %in% colnames(data_wide), o3, rep(0, nrow(data_wide))),
                     pm10 = ifelse(rep("pm10", nrow(data_wide)) %in% colnames(data_wide), pm10, rep(0, nrow(data_wide))),
                     pm25 = ifelse(rep("pm25", nrow(data_wide)) %in% colnames(data_wide), pm25, rep(0, nrow(data_wide))),
                     so2 = ifelse(rep("so2", nrow(data_wide)) %in% colnames(data_wide), so2, rep(0, nrow(data_wide))))
  
  print(line_plot_multiple(paste("Airpollution Data -", unique(data_plot$sub_region_1)[i]), outpath,data_wide$Date,"Date", "Airpollution", names_y=unique(data_plot$parameter), 
                           y_percent=F, legend=T, data_wide$co / mean(data_wide$co, na.rm=T), data_wide$no2 / mean(data_wide$no2, na.rm=T), data_wide$o3 / mean(data_wide$o3, na.rm=T), 
                           data_wide$pm10 / mean(data_wide$pm10, na.rm=T), data_wide$pm25 / mean(data_wide$pm25, na.rm=T), data_wide$so2 / mean(data_wide$so2, na.rm=T), data_wide$bc / mean(data_wide$bc, na.rm=T)))
}
  
  
# Difference Airpollution to previous year ----
  
  # Plot worldwide 
  data_wide = group_by(open_state_clean, parameter, Date) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year) %>%
    pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = c("value", "value_difference", "value_last_year")) %>%
    ungroup()
  
  print(line_plot_multiple(paste("Difference Previous Year - World", unique(open_state_clean$parameter)[1]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[1],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_co, data_wide$value_last_year_co,data_wide$value_difference_co))
  print(line_plot_multiple(paste("Difference Previous Year - World", unique(open_state_clean$parameter)[2]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[2],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_no2, data_wide$value_last_year_no2, data_wide$value_difference_no2))
  print(line_plot_multiple(paste("Difference Previous Year - World", unique(open_state_clean$parameter)[3]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[3],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_o3, data_wide$value_last_year_o3,data_wide$value_difference_o3))
  print(line_plot_multiple(paste("Difference Previous Year - World", unique(open_state_clean$parameter)[4]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[4],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm10, data_wide$value_last_year_pm10,data_wide$value_difference_pm10))
  print(line_plot_multiple(paste("Difference Previous Year - World", unique(open_state_clean$parameter)[5]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[5],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_so2 , data_wide$value_last_year_so2, data_wide$value_difference_so2 ))
  print(line_plot_multiple(paste("Difference Previous Year - World", unique(open_state_clean$parameter)[6]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[6],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm25, data_wide$value_last_year_pm25, data_wide$value_difference_pm25 ))
  print(line_plot_multiple(paste("Difference Previous Year - World", unique(open_state_clean$parameter)[7]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[7],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_bc, data_wide$value_last_year_bc, data_wide$value_difference_bc ))
  
  # Plot one region 
  select_region = "WESTERN EUROPE"
  data_wide = filter(open_state_clean, Region ==select_region )
  
  data_wide = group_by(data_wide, parameter, Date) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year) %>%
    pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = c("value", "value_difference", "value_last_year")) %>%
    ungroup()
  
  print(line_plot_multiple(paste("Difference Previous Year -",select_region, unique(open_state_clean$parameter)[1]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[1],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_co, data_wide$value_last_year_co,data_wide$value_difference_co))
  print(line_plot_multiple(paste("Difference Previous Year -",select_region, unique(open_state_clean$parameter)[2]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[2],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_no2, data_wide$value_last_year_no2, data_wide$value_difference_no2))
  print(line_plot_multiple(paste("Difference Previous Year -",select_region, unique(open_state_clean$parameter)[3]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[3],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_o3, data_wide$value_last_year_o3,data_wide$value_difference_o3))
  print(line_plot_multiple(paste("Difference Previous Year -",select_region, unique(open_state_clean$parameter)[4]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[4],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm10, data_wide$value_last_year_pm10,data_wide$value_difference_pm10))
  print(line_plot_multiple(paste("Difference Previous Year -",select_region, unique(open_state_clean$parameter)[5]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[5],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_so2 , data_wide$value_last_year_so2, data_wide$value_difference_so2 ))
  print(line_plot_multiple(paste("Difference Previous Year -",select_region, unique(open_state_clean$parameter)[6]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[6],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm25, data_wide$value_last_year_pm25, data_wide$value_difference_pm25 ))
  print(line_plot_multiple(paste("Difference Previous Year -",select_region, unique(open_state_clean$parameter)[7]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[7],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_bc, data_wide$value_last_year_bc, data_wide$value_difference_bc ))
  
  # Plot one country 
  select_country = "it"
  data_wide = filter(open_state_clean, CountryCode ==select_country)
  
  data_wide = group_by(data_wide, parameter, Date) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year) %>%
    pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = c("value", "value_difference", "value_last_year")) %>%
    ungroup()
  
  print(line_plot_multiple(paste("Difference Previous Year -",select_country, unique(open_state_clean$parameter)[2]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[2],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_no2, data_wide$value_last_year_no2, data_wide$value_difference_no2))
  print(line_plot_multiple(paste("Difference Previous Year -",select_country, unique(open_state_clean$parameter)[3]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[3],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_o3, data_wide$value_last_year_o3,data_wide$value_difference_o3))
  print(line_plot_multiple(paste("Difference Previous Year -",select_country, unique(open_state_clean$parameter)[4]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[4],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm10, data_wide$value_last_year_pm10,data_wide$value_difference_pm10))
  print(line_plot_multiple(paste("Difference Previous Year -",select_country, unique(open_state_clean$parameter)[5]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[5],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_so2 , data_wide$value_last_year_so2, data_wide$value_difference_so2 ))
  print(line_plot_multiple(paste("Difference Previous Year -",select_country, unique(open_state_clean$parameter)[6]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[6],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm25, data_wide$value_last_year_pm25, data_wide$value_difference_pm25 ))
  print(line_plot_multiple(paste("Difference Previous Year -",select_country, unique(open_state_clean$parameter)[7]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[7],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_bc, data_wide$value_last_year_bc, data_wide$value_difference_bc ))
  
  # Plot one subregion 
  select_subregion = "bremen"
  data_wide = filter(open_state_clean, sub_region_1 ==select_subregion)
  
  data_wide = group_by(data_wide, parameter, Date) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year) %>%
    pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = c("value", "value_difference", "value_last_year")) %>%
    ungroup()
  
  print(line_plot_multiple(paste("Difference Previous Year -",select_subregion, unique(open_state_clean$parameter)[2]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[2],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_no2, data_wide$value_last_year_no2, data_wide$value_difference_no2))
  print(line_plot_multiple(paste("Difference Previous Year -",select_subregion, unique(open_state_clean$parameter)[3]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[3],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_o3, data_wide$value_last_year_o3,data_wide$value_difference_o3))
  print(line_plot_multiple(paste("Difference Previous Year -",select_subregion, unique(open_state_clean$parameter)[4]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[4],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm10, data_wide$value_last_year_pm10,data_wide$value_difference_pm10))
  print(line_plot_multiple(paste("Difference Previous Year -",select_subregion, unique(open_state_clean$parameter)[5]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[5],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_so2 , data_wide$value_last_year_so2, data_wide$value_difference_so2 ))
  print(line_plot_multiple(paste("Difference Previous Year -",select_subregion, unique(open_state_clean$parameter)[6]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[6],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_pm25, data_wide$value_last_year_pm25, data_wide$value_difference_pm25 ))
  print(line_plot_multiple(paste("Difference Previous Year -",select_subregion, unique(open_state_clean$parameter)[7]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(open_state_clean$parameter)[7],"last_year","diff"), 
                           y_percent=F, legend=T, data_wide$value_bc, data_wide$value_last_year_bc, data_wide$value_difference_bc ))
  

  
  # Use Standardized Data (z-scores)
    # plot regions
    # plot each region
    for (i in 1:length(unique(country_difference_data$parameter))) {
      data_plot = filter(country_difference_data_standardized, parameter == unique(country_difference_data$parameter)[i])
      
      title=paste("Airpollution Standardized Regions",unique(country_difference_data$parameter)[i])
      print(ggplot(data_plot, aes(x = Date))+
              geom_line(aes(y=value, color = Region))+
              ggtitle(paste(title,sep=" ")) +
              theme(plot.title = element_text(size=10, face="bold"))+
              theme(axis.text=element_text(size=10),
                    axis.title=element_text(size=10,face="bold"))+
              ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
      
      title=paste("Airpollution Difference Previous Year Regions Standardized -",unique(country_difference_data$parameter)[i])
      print(ggplot(data_plot, aes(x = Date))+
              geom_line(aes(y=value_difference, color = Region))+
              ggtitle(paste(title,sep=" ")) +
              theme(plot.title = element_text(size=10, face="bold"))+
              theme(axis.text=element_text(size=10),
                    axis.title=element_text(size=10,face="bold"))+
              ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
    }
  
    # plot each country
    for (i in 1:length(unique(country_difference_data$Region))) {
      data_plot = filter(country_difference_data_standardized, Region == unique(country_difference_data$Region)[i])
      
      title=paste("Airpollution Standardized",unique(country_difference_data$Region)[i])
      print(ggplot(data_plot, aes(x = Date))+
              geom_line(aes(y=value, color = CountryCode))+
              facet_wrap(~parameter) +
              ggtitle(paste(title,sep=" ")) +
              theme(plot.title = element_text(size=10, face="bold"))+
              theme(axis.text=element_text(size=10),
                    axis.title=element_text(size=10,face="bold"))+
              ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
      
      title=paste("Airpollution Difference Previous Year Standardized-",unique(country_difference_data$Region)[i])
      print(ggplot(data_plot, aes(x = Date))+
              geom_line(aes(y=value_difference, color = CountryCode))+
              facet_wrap(~parameter) +
              ggtitle(paste(title,sep=" ")) +
              theme(plot.title = element_text(size=10, face="bold"))+
              theme(axis.text=element_text(size=10),
                    axis.title=element_text(size=10,face="bold"))+
              ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
    }
  
  
  # normal data
  for (i in 1:length(unique(country_difference_data$Region))) {
    data_plot = filter(country_difference_data, Region == unique(country_difference_data$Region)[i])
    
    title=paste("Airpollution",unique(country_difference_data$Region)[i])
    print(ggplot(data_plot, aes(x = Date))+
            geom_line(aes(y=value, color = CountryCode))+
            facet_wrap(~parameter) +
            ggtitle(paste(title,sep=" ")) +
            theme(plot.title = element_text(size=10, face="bold"))+
            theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10,face="bold"))+
            ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
    
    title=paste("Airpollution Difference Previous Year -",unique(country_difference_data$Region)[i])
    print(ggplot(data_plot, aes(x = Date))+
            geom_line(aes(y=value_difference, color = CountryCode))+
            facet_wrap(~parameter) +
            ggtitle(paste(title,sep=" ")) +
            theme(plot.title = element_text(size=10, face="bold"))+
            theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10,face="bold"))+
            ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
  }
  
  # subregion difference to country 
  
  list_countries = c("us", "de", "it", "cn")
  
  subregion_difference_data = filter(open_state_clean, CountryCode %in% list_countries) %>%
    group_by(parameter, Date, Region,CountryCode, sub_region_1) %>%
    summarize(value = mean(value, na.rm=T), value_last_year = mean(value_last_year, na.rm=T)) %>%
    mutate(value_indicator = ifelse(is.na(value),1,0)) %>%
    mutate(value_last_year_indicator = ifelse(is.na(value_last_year),1,0)) %>%
    ungroup(Date) %>%
    arrange(Date) %>%
    mutate(value = ifelse((rollapply(value_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_last_year = ifelse((rollapply(value_last_year_indicator,parms_ma$medium,sum,  na.rm = TRUE, align = "center", fill = NA))<parms_ma$max_na_medium,  rollapply(value_last_year,parms_ma$medium,mean, align = "center",  na.rm = TRUE, fill = NA), NA)) %>%
    mutate(value_difference = value - value_last_year)
  
  for (i in 1:length(list_countries)) {
    data_plot = filter(subregion_difference_data, CountryCode == list_countries[i])
    
    title=paste("Airpollution",list_countries[i])
    print(ggplot(data_plot, aes(x = Date))+
            geom_line(aes(y=value, color = sub_region_1))+
            facet_wrap(~parameter) +
            ggtitle(paste(title,sep=" ")) +
            theme(plot.title = element_text(size=10, face="bold"))+
            theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10,face="bold"))+
            ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
    
    title=paste("Airpollution Difference Previous Year -",list_countries[i])
    print(ggplot(data_plot, aes(x = Date))+
            geom_line(aes(y=value_difference, color = sub_region_1))+
            facet_wrap(~parameter) +
            ggtitle(paste(title,sep=" ")) +
            theme(plot.title = element_text(size=10, face="bold"))+
            theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10,face="bold"))+
            ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
  }
  
  for (i in 1:length(list_countries)) {
    data_plot = filter(subregion_difference_data, CountryCode == list_countries[i])
    
    title=paste("Airpollution",list_countries[i])
    print(ggplot(data_plot, aes(x = Date))+
            geom_line(aes(y=value, color = sub_region_1))+
            facet_wrap(~parameter) +
            ggtitle(paste(title,sep=" ")) +
            theme(plot.title = element_text(size=10, face="bold"))+
            theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10,face="bold"))+
            ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
    
    title=paste("Airpollution Difference Previous Year -",list_countries[i])
    print(ggplot(data_plot, aes(x = Date))+
            geom_line(aes(y=value_difference, color = sub_region_1))+
            facet_wrap(~parameter) +
            ggtitle(paste(title,sep=" ")) +
            theme(plot.title = element_text(size=10, face="bold"))+
            theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10,face="bold"))+
            ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
  }


# Effect Estimation ----

  # take means of pollution data and stringency before and after ovid
    start_covid_timeframe = as.Date('2020-03-20')
    mid_covid_timeframe = start_covid_timeframe + days(round(parms_ma$medium/2,0))
    end_covid_timeframe = start_covid_timeframe + days(parms_ma$medium)
    
    # short MA-timeframe for stringency before covid because data starts only on 2020-02-15
    start_no_covid_timeframe = as.Date('2020-02-15')
    mid_no_covid_timeframe = start_no_covid_timeframe + days(2)
    end_no_covid_timeframe = start_no_covid_timeframe + days(4)
    
    # CHOICE: paramters: edit params to only get effects for certain regions / countries
    region_effect_list = unique(global_mobility_report_clean_stringency_index$Region)
    region_effect_list = "WESTERN EUROPE"
    country_effect_list = unique(global_mobility_report_clean_stringency_index$CountryCode)
  
    # World --> Country
      # airpollution data. 
        # CHOICE: country_difference_data, subregion_difference_data, country_difference_data_standardized, subregion_difference_data_standardized
        effect_data_raw = subregion_difference_data_standardized
        effect_data_raw = effect_data_raw

        # Merge with mobility data
        merged_data_effect = inner_join(global_mobility_report_clean_stringency_index, select(effect_data_raw, -c("CountryCode", "Region")), by = c("sub_region_1","Date")) 
        summary(merged_data_effect)
      
      # cut timeframe
      effect_data = filter(merged_data_effect, (Date >=start_covid_timeframe & Date <= end_covid_timeframe)| (Date >=start_no_covid_timeframe & Date <= end_no_covid_timeframe)) %>%
        mutate(covid_time = ifelse((Date >=start_covid_timeframe & Date <= end_covid_timeframe), 1,0)) %>%
        filter(Region %in% region_effect_list) %>%
        filter(CoutryCode %in% country_effect_list)
      
      # take means / values at covid
      effect_data = effect_data %>%
        group_by(CountryCode, Region, sub_region_1,parameter, Date, covid_time) %>%
        summarize(StringencyIndex = mean(StringencyIndex, na.rm=T), value = mean(value_difference  , na.rm=T)) %>% # summarize over country / day
        group_by(CountryCode, sub_region_1,Region ,parameter, covid_time) %>%
        mutate(StringencyIndex = mean(StringencyIndex, na.rm=T)) %>% # take mean stringency index for before / after covid. value is already MA (from input)
        filter(Date== mid_covid_timeframe | Date == mid_no_covid_timeframe) %>%
        arrange(CountryCode,Region,sub_region_1,parameter,covid_time)
    
      # calculate difference in value and covid
      # !CAREFUL: due to missing observations, this is not the same plot / regressions with cross section before and in covid
    
        effect_data_difference  = effect_data %>%
          group_by(CountryCode, Region , sub_region_1,parameter) %>%
          arrange(CountryCode, parameter, sub_region_1,covid_time) %>%
          mutate(StringencyIndex_difference = StringencyIndex-lag(StringencyIndex),value_difference = value-lag(value)) %>%
          filter(covid_time==1)
        
        summary(effect_data[effect_data$covid_time==0,])
        summary(effect_data[effect_data$covid_time==1,])
        
        # scatter plot difference in pollution and difference in stringency to investigate effect
        for (i in 1:length(unique(effect_data_difference$parameter))) {
          effect_data_scatter  = effect_data_difference %>%
            filter(parameter ==unique(effect_data$parameter)[i])
          
          print(ggplot(effect_data_scatter, aes(x = StringencyIndex_difference, y = value_difference)) +
                  geom_point(aes(col = Region)) + 
                  geom_smooth(method='lm') + 
                  ggtitle(unique(effect_data_difference$parameter)[i]))
        }
  
    
    # difference in difference estimation for all pollution parameters
    parameters_pollution = unique(effect_data$parameter)
    diff_diff_model = list()
    for (i in 1:length(parameters_pollution)) {
      
      model_data = filter(effect_data, parameter == parameters_pollution[i])
      print(unique(effect_data$parameter)[i])
      print(summary(lm(value ~ covid_time + StringencyIndex+covid_time*StringencyIndex, data = model_data)))
    }
    
    parameters_pollution = unique(effect_data_difference$parameter)
    diff_diff_model = list()
    for (i in 1:length(parameters_pollution)) {
      
      model_data = filter(effect_data_difference, parameter == parameters_pollution[i])
      print(unique(effect_data_difference$parameter)[i])
      print(summary(lm(value_difference ~ StringencyIndex_difference, data = model_data)))
    }
  
  
  
