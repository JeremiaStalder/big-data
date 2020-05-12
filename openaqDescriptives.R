# Descriptives Airpollution Data

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
outpath = "./output/openaqDescriptives/" # output

# parameters
  # rolling average for value to figure out seasonal patterns. 
  parms_ma = list(15, 30, 60, 12, 20, 45)
  names(parms_ma) = c("short","medium","long","max_na_short","max_na_medium","max_na_long")

# Import Data ----
  open_state_clean = read_csv("data/output_openaqData/open_state_clean.csv", 
                                   col_types = cols(Date = col_date(format = "%Y-%m-%d"))) 
  country_difference_data = read_csv("data/output_openaqData/country_difference_data.csv", 
                              col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                               value = col_double(),
                                               value_last_year = col_double(),
                                               value_indicator = col_double(),
                                               value_last_year_indicator = col_double(),
                                               value_difference = col_double()))

  country_difference_data_standardized = read_csv("data/output_openaqData/country_difference_data_standardized.csv", 
                                                  col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                                   value = col_double(),
                                                                   value_last_year = col_double(),
                                                                   value_indicator = col_double(),
                                                                   value_last_year_indicator = col_double(),
                                                                   value_difference = col_double()))
  
  subregion_difference_data = read_csv("data/output_openaqData/subregion_difference_data.csv", 
                                       col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                        value = col_double(),
                                                        value_last_year = col_double(),
                                                        value_indicator = col_double(),
                                                        value_last_year_indicator = col_double(),
                                                        value_difference = col_double()))
  
  subregion_difference_data_standardized = read_csv("data/output_openaqData/subregion_difference_data_standardized.csv", 
                                                    col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                                     value = col_double(),
                                                                     value_last_year = col_double(),
                                                                     value_indicator = col_double(),
                                                                     value_last_year_indicator = col_double(),
                                                                     value_difference = col_double()))
  
  global_mobility_report_clean_stringency_index = read_csv("data/output_openaqData/global_mobility_report_clean_to_merge_with_openaq.csv", 
                                                           col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                           CountryName = col_character(), 
                                                           grocery_and_pharmacy = col_double(), 
                                                           parks = col_double(), residential = col_double(), 
                                                           retail_and_recreation = col_double(), 
                                                           sub_region_1 = col_character(), transit_stations = col_double(), 
                                                           workplaces = col_double()))

  
# Raw Data ----   
  
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