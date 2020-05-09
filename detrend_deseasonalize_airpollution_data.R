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
  
  setwd("~/GitHub/big-data") # setwd
  source("functions.R") # functions
  outpath = outpath = "./output/detend_deseasonalize_airpollution_data/" # output
  

# import data----

  # airpollution data
    # load data from locally or from database (save file from database).
    # Note: local file implemented to prevent waiting times from database
    load_locally = 1

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
    global_mobility_report_clean_stringency_index <- read_csv("data/clean/global_mobility_report_clean_stringency_index.csv", 
                                                              col_types = cols(Date = col_date(format = "%Y-%m-%d")))
    
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
    
  # convert units from ppm to microgram/m3
    # inputs for function
      list_particles_convert = unique(select(filter(openaq_state, unit == "ppm"), parameter))$parameter
      
############## INSERT Weather DATA WHEN AVAILABLE ----
      particle  = openaq_state$parameter[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ]
      tempF = rep(NA, length(particle))
      pressure_millibars_to_tenth = rep(NA, length(particle))
      concentration_pmm = openaq_state$value[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ]
    # call function
      openaq_state$value[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ] = ppm_to_microgram(particle,tempF, pressure_millibars_to_tenth, concentration_pmm, unit_conversion_table)
      openaq_state$unit[(openaq_state$parameter %in% list_particles_convert) & (openaq_state$unit =="ppm") ] = "microgram_per_m3_transformed"
      
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
  

  table1 =  filter(openaq_state, parameter=="o3" & Date >= '2019-06-01') %>%
    group_by(Date, CountryCode,sub_region_1, parameter, unit) %>%
    summarize(value = mean(value))
  
  table11 = filter(table1, parameter=="o3" & Date == '2019-06-10') 
  summary(table11)
  
  openaq_state_ma_monthly = group_by(openaq_state, CountryCode,sub_region_1, parameter, unit) %>%
    arrange(Date) %>%
    mutate(value = rollapply(value,60,mean, align = "center",  na.rm = TRUE, fill = NA)) %>%
    ungroup()# get monthly moving average
  

  
  # openaq_state_ma_monthly = group_by(openaq_state, CountryCode,sub_region_1, parameter, unit) %>%
  #   arrange(Date) %>%
  #   mutate(value = ifelse(rollapply(value, 60, is.na,align ="center")<30,  rollapply(value,60,mean, align = "center", fill=NA), NA)) %>%
  #   ungroup()# get monthly moving average
  
  table22 =  filter(openaq_state_ma_monthly, parameter=="o3" & Date == '2019-06-10')
  summary(table22)
  
  table2 =  filter(openaq_state_ma_monthly, parameter=="o3" & Date == '2019-06-10') %>%
    group_by(Date) %>%
    summarize(value = mean(value, na.rm =T))
  table2
  
  # save datasets. use last version for models, plots etc
    openaq_state_clean_daily = openaq_state # raw daily data
    openaq_state_clean_ma = openaq_state_ma_monthly
    
    open_state_clean = openaq_state_clean_ma
  
# Merge Covid & Airpollution ----
  
  # replace region with country in openaq if no match in mobility data
  indicator_change_region_name = sort(match(unique(global_mobility_report_clean_stringency_index$sub_region_1), unique(open_state_clean$sub_region_1))) # identify matched regions
  not_matched_regions = unique(openaq_state_ma_monthly$sub_region_1)[-indicator_change_region_name] # identify non-matched regions
  open_state_clean[(open_state_clean$sub_region_1 %in% not_matched_regions),]$sub_region_1 = open_state_clean[(open_state_clean$sub_region_1 %in% not_matched_regions),]$CountryCode
  
  # test match
  indicator_change_region_name_1 = sort(match(unique(global_mobility_report_clean_stringency_index$sub_region_1), unique(open_state_clean$sub_region_1))) 
  not_matched_regions_1 = unique(open_state_clean$sub_region_1)[-indicator_change_region_name_1]
  print(paste("Merging:",length(not_matched_regions_1), "Subregions are not matched"))
  
  # merge by CountryCode, sub_region_1 and Date (CountryCode should not be necessary if no mistakes before)
  merged_data = inner_join(global_mobility_report_clean_stringency_index, open_state_clean, by = c("CountryCode", "sub_region_1", "Date")) 
  summary(merged_data)
  
  regions = unique(merged_data$sub_region_1)  
  
  
# Airpollution Data Step 2 ---- 
    # add regions to entire openaq_state_ma_monthly dataset
    list_country_code_region = select(global_mobility_report_clean_stringency_index, CountryCode, Region) %>%
      distinct()
    open_state_clean = left_join(open_state_clean, list_country_code_region, by=("CountryCode"))
    
    # Average over new subregions after merging
    open_state_clean = group_by(open_state_clean, CountryCode, Date, sub_region_1, parameter, unit) %>%
      summarize(value = mean(value, na.rm=T)) %>%
      ungroup()
    
  
# Descriptive Airpollution ----   
 
  # Plot pollution data
    
  # worldwide
  data_wide = group_by(open_state_clean, parameter, Date) %>%
   summarize(value = mean(value, na.rm=T)) %>%
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
  
  print(line_plot_multiple(paste("Airpollution Data - World"), outpath,data_wide$Date,"Date", "Airpollution", names_y=unique(openaq_state_ma_monthly$parameter), 
                           y_percent=F, legend=T, data_wide$co / mean(data_wide$co, na.rm=T), data_wide$no2 / mean(data_wide$no2, na.rm=T), data_wide$o3 / mean(data_wide$o3, na.rm=T), 
                           data_wide$pm10 / mean(data_wide$pm10, na.rm=T), data_wide$pm25 / mean(data_wide$pm25, na.rm=T), data_wide$so2 / mean(data_wide$so2, na.rm=T), data_wide$bc / mean(data_wide$bc, na.rm=T)))
  
  print(line_plot_multiple(paste("Airpollution Data - World"), outpath,data_wide$Date,"Date", "Airpollution", names_y=unique(openaq_state_ma_monthly$parameter), 
                           y_percent=F, legend=T, data_wide$o3 / mean(data_wide$o3, na.rm=T)))

  # regions
  data_plot = group_by(open_state_clean, parameter, Date, Region) %>%
    summarize(value = mean(value, na.rm=T))
  
  for (i in 1:length(unique(data_plot$Region))) {
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

  # countries
  
  list_countries = c("us", "de", "it", "cn","af","ae", "au")
  
  data_plot = filter(open_state_clean, CountryCode %in% list_countries) %>%
    group_by(parameter, Date, CountryCode) %>%
    summarize(value = mean(value, na.rm=T))
  
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
    summarize(value = mean(value, na.rm=T))
  
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
  
  
  # difference to previous year
   open_state_clean_previous_year = mutate(open_state_clean, Date = Date+years(1))  %>%
      rename(value_last_year = value)# shift date 1 year ahead to merge
    
   open_state_clean_difference = inner_join(open_state_clean, select(open_state_clean_previous_year, Date, CountryCode, sub_region_1, parameter,value_last_year), by = c("Date", "CountryCode", "sub_region_1", "parameter"))
   open_state_clean_difference$value_difference = open_state_clean_difference$value- open_state_clean_difference$value_last_year

    t1 = group_by(open_state_clean, Date,parameter)  %>%
      summarize(value = mean(value, na.rm =T))
    
    t2 = group_by(open_state_clean, Date, parameter)  %>%
    summarize(value_last_year = mean(value_last_year, na.rm =T))
    
    open_state_clean_difference = left_join(t1, select(t2, Date, parameter,value_last_year), by = c("Date", "parameter"))
    open_state_clean_difference$value_difference = open_state_clean_difference$value- open_state_clean_difference$value_last_year
    
    
    data_wide = group_by(open_state_clean_difference, parameter, Date) %>%
      summarize(value_difference = mean(value_difference, na.rm=T), value = mean(value, na.rm=T)) %>%
      pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = c("value", "value_difference")) %>%
      ungroup()
    # 
    # table_3 = filter(data_wide, Date >='2020-04-01', CountryCode == "de")
    # data_wide = group_by(openaq_state_ma_monthly_difference, parameter, Date, CountryCode) %>%
    #   summarize(value_difference = mean(value_difference, na.rm=T), value = mean(value, na.rm=T)) %>%
    #   ungroup()
    # 
    # table_1 = filter(openaq_state_ma_monthly_difference, Date >= '2019-02-01' & CountryCode == "us" & parameter == "co")
    # table_2 = filter(openaq_state_ma_monthly_difference, Date >= '2020-02-01' & CountryCode == "us" & parameter == "co")
    # 
    # table_1 = filter(data_wide, Date >= '2019-02-01' & CountryCode == "us" & parameter == "co")
    # table_2 = filter(data_wide, Date >= '2020-02-01' & CountryCode == "us" & parameter == "co")
    # head(table_1)
    # head(table_2)
    
    # openaq_state_ma_monthly_difference$value
    # openaq_state_ma_monthly_difference$value
    # 
    # summary(openaq_state_ma_monthly_difference)
    
      # worldwide
      data_wide = group_by(open_state_clean_difference, parameter, Date) %>%
        summarize(value_difference = mean(value_difference, na.rm=T), value = mean(value, na.rm=T)) %>%
        pivot_wider(id_cols = c("Date", "parameter"), names_from=parameter, values_from = c("value", "value_difference")) %>%
        ungroup()
      
      data_wide = group_by(open_state_clean_difference, parameter, Date) %>%
        summarize(value_difference = mean(value_difference, na.rm=T), value = mean(value, na.rm=T),  value_last_year = mean(value_last_year, na.rm=T)) 
      table_1 = filter(data_wide, Date >= '2019-04-01' & parameter == "co")
      table_2 = filter(data_wide, Date >= '2020-03-30' & parameter == "co")
      head(table_1)
      head(table_2)
      
      # add 0s for non-measures parms
      data_wide = mutate(data_wide, bc = ifelse(rep("bc", nrow(data_wide)) %in% colnames(data_wide), bc, rep(0, nrow(data_wide))), 
                         co = ifelse(rep("co", nrow(data_wide)) %in% colnames(data_wide), co, rep(0, nrow(data_wide))), 
                         no2 = ifelse(rep("no2", nrow(data_wide)) %in% colnames(data_wide), no2, rep(0, nrow(data_wide))),
                         o3 = ifelse(rep("o3", nrow(data_wide)) %in% colnames(data_wide), o3, rep(0, nrow(data_wide))),
                         pm10 = ifelse(rep("pm10", nrow(data_wide)) %in% colnames(data_wide), pm10, rep(0, nrow(data_wide))),
                         pm25 = ifelse(rep("pm25", nrow(data_wide)) %in% colnames(data_wide), pm25, rep(0, nrow(data_wide))),
                         so2 = ifelse(rep("so2", nrow(data_wide)) %in% colnames(data_wide), so2, rep(0, nrow(data_wide))))
      
      print(line_plot_multiple(paste("Difference Previous Year - World", unique(openaq_state_ma_monthly$parameter)[1]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(openaq_state_ma_monthly$parameter)[1],"diff"), 
                               y_percent=F, legend=T, data_wide$value_co, data_wide$value_difference_co))
      
      print(line_plot_multiple(paste("Difference Previous Year - World", unique(openaq_state_ma_monthly$parameter)[2]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(openaq_state_ma_monthly$parameter)[2],"diff"), 
                               y_percent=F, legend=T, data_wide$value_no2, data_wide$value_difference_no2))
      
      
      print(line_plot_multiple(paste("Difference Previous Year - World", unique(openaq_state_ma_monthly$parameter)[3]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(openaq_state_ma_monthly$parameter)[3],"diff"), 
                               y_percent=F, legend=T, data_wide$value_o3 ,data_wide$value_difference_o3))
      
      
      print(line_plot_multiple(paste("Difference Previous Year - World", unique(openaq_state_ma_monthly$parameter)[4]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(openaq_state_ma_monthly$parameter)[4],"diff"), 
                               y_percent=F, legend=T, data_wide$value_pm10,data_wide$value_difference_pm10))
      
      
      print(line_plot_multiple(paste("Difference Previous Year - World", unique(openaq_state_ma_monthly$parameter)[5]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(openaq_state_ma_monthly$parameter)[5],"diff"), 
                               y_percent=F, legend=T, data_wide$value_so2 , data_wide$value_difference_so2 ))
      
      
      print(line_plot_multiple(paste("Difference Previous Year - World", unique(openaq_state_ma_monthly$parameter)[6]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(openaq_state_ma_monthly$parameter)[6],"diff"), 
                               y_percent=F, legend=T, data_wide$value_pm25, data_wide$value_difference_pm25 ))
      
      print(line_plot_multiple(paste("Difference Previous Year - World", unique(openaq_state_ma_monthly$parameter)[7]), outpath,data_wide$Date,"Date", "Airpollution", names_y=c(unique(openaq_state_ma_monthly$parameter)[7],"diff"), 
                               y_percent=F, legend=T, data_wide$value_bc, data_wide$value_difference_bc ))
      
  
# Seasonality Approaches ----
# test decompose

# produce test time series object
drift = 1
sigma = 5
number_obs = 50

test_data = ts(sigma * sin(c(1:number_obs)*pi*0.5+0.5*pi) + drift *seq(number_obs), start =1, end = number_obs, frequency = number_obs)
test_data_diff = test_data - stats::lag(test_data)

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

arima = auto.arima(y = test_data, seasonal=F, ic="bic")

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


# Effect Calculation ----

# take means of pollution data and stringency measures before Covid and in Covid
start_covid_timeframe = '2020-03-15'
end_covid_timeframe = '2020-03-31'

start_no_covid_timeframe = '2020-02-15'
end_no_covid_timeframe = '2020-02-28'


effect_data = filter(merged_data, (Date >=start_covid_timeframe & Date <= end_covid_timeframe)| (Date >=start_no_covid_timeframe & Date <= end_no_covid_timeframe)) %>%
  mutate(covid_time = ifelse((Date >=start_covid_timeframe & Date <= end_covid_timeframe), 1,0)) %>%
  group_by(sub_region_1, parameter, covid_time) %>%
  summarize(value = mean(value), StringencyIndex = mean(StringencyIndex)) %>%
  arrange(sub_region_1,parameter,covid_time)

summary(effect_data[effect_data$covid_time==0,])
summary(effect_data[effect_data$covid_time==1,])

# difference in difference estimation for all pollution parameters
parameters_pollution = unique(merged_data$parameter)
diff_diff_model = list()
for (i in 1:length(parameters_pollution)) {
  
  model_data = filter(effect_data, parameter == parameters_pollution[i])
  
  print(summary(lm(value ~ covid_time + StringencyIndex, data = model_data)))
  # diff_diff_model[i] = lm(value ~ covid_time + StringencyIndex + covid_time*StringencyIndex, data = model_data)
  # print(summary(diff_diff_model[[i]]))
}



