# Effect Estimation: Stringency -> Covid 

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

# Effect Estimation ----

  # Settings for effect estimation:
    # CHOICE: take means of pollution data and stringency before and after ovid
    start_covid_timeframe = as.Date('2020-03-20')
    mid_covid_timeframe = start_covid_timeframe + days(round(parms_ma$medium/2,0))
    end_covid_timeframe = start_covid_timeframe + days(parms_ma$medium)
    
    start_no_covid_timeframe = as.Date('2020-02-15')
    mid_no_covid_timeframe = start_no_covid_timeframe + days(2)
    end_no_covid_timeframe = start_no_covid_timeframe + days(4)
    
    # CHOICE: paramters: edit params to only get effects for certain regions / countries
    region_effect_list = unique(global_mobility_report_clean_stringency_index$Region)
    country_effect_list = unique(global_mobility_report_clean_stringency_index$CountryCode)
    
    # CHOICE: Data - country_difference_data, subregion_difference_data, country_difference_data_standardized, subregion_difference_data_standardized
    effect_data_raw = subregion_difference_data_standardized
    effect_data_raw = effect_data_raw
  
  # Get data for effect estimation
    # Merge with mobility data
    merged_data_effect = inner_join(global_mobility_report_clean_stringency_index, select(effect_data_raw, -c("CountryCode", "Region")), by = c("sub_region_1","Date")) 
    colnames(merged_data_effect)
    
    # cut timeframe
    effect_data = filter(merged_data_effect, (Date >=start_covid_timeframe & Date <= end_covid_timeframe)| (Date >=start_no_covid_timeframe & Date <= end_no_covid_timeframe)) %>%
      mutate(covid_time = ifelse((Date >=start_covid_timeframe & Date <= end_covid_timeframe), 1,0)) %>%
      filter(Region %in% region_effect_list) %>%
      filter(CountryCode %in% country_effect_list)
    
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
  
  
  # Effect estimation
  
    # Visual: scatter plot difference in pollution and difference in stringency to investigate effect
    for (i in 1:length(unique(effect_data_difference$parameter))) {
      effect_data_scatter  = effect_data_difference %>%
        filter(parameter ==unique(effect_data$parameter)[i])
      
      print(ggplot(effect_data_scatter, aes(x = StringencyIndex_difference, y = value_difference)) +
              geom_point(aes(col = Region)) + 
              geom_smooth(method='lm') + 
              ggtitle(unique(effect_data_difference$parameter)[i]))
    }
  
  
    # Difference in Difference estimation for all pollution parameters
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
  
  
  
