# detrend and deseasonalize air pollution data

library(tidyverse)
library(readr)
library(zoo)
library(RMariaDB) 

# import data----
  # connect to database 
  bigdatadb <- dbConnect(RMariaDB::MariaDB(), user='bdauser', password='BDA4ever!!!', dbname='bigdatadb', host='35.193.193.138')
  dbListTables(bigdatadb)
  
  # send query
  query = "SELECT * FROM openaq_state WHERE date <= '2020-04-30';"
  openaq_state_original = dbGetQuery(bigdatadb,query)
  
  # disconnect
  dbDisconnect(bigdatadb)
  
  # import mobililty data
  global_mobility_report_clean <- read_csv("data/clean/global_mobility_report_clean.csv", 
                                           col_types = cols(Date = col_date(format = "%Y-%m-%d")))
  global_mobility_report_clean = mutate(global_mobility_report_clean, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) # redo because "NA" is imported as NA
  global_mobility_report_clean = mutate(global_mobility_report_clean, sub_region_1 = ifelse(is.na(sub_region_1),CountryCode, sub_region_1)) # insert CountryCode as region to merge. Reason: if subregion in open aq missing, then country code is inserted
  
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
  a = unique(openaq_state_clean[,c("sub_region_1", "CountryCode")])[,-indicator_change_region_name] # identify non-matched regions and their country code
  
  filter_mobility = filter(global_mobility_report_clean, CountryCode =="CH") 
  a
  filter_mobility
  
  # merge by sub_region_1 and Date
  merged_data = inner_join(global_mobility_report_clean, openaq_state_clean, by = c("sub_region_1", "Date")) 
  # summary(merged_data)
  # summary(is.na(merged_data))
  
  print("number of regions")
  length(unique(merged_data$sub_region_1))
  
  