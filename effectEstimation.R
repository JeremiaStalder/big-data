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
outpath = "./output/openaqEffectEstimation/" # output

# parameters
  # rolling average for value to figure out seasonal patterns. 
  parms_ma = list(15, 30, 60, 12, 20, 45)
  names(parms_ma) = c("short","medium","long","max_na_short","max_na_medium","max_na_long")

# Import Data ----
  open_state_clean = read_csv("data/output_openaqData/open_state_clean.csv", 
                              col_types = cols(Date = col_date(format = "%Y-%m-%d"))) 
  
  openaq_state_clean_ma = read_csv("data/output_openaqData/openaq_state_clean_ma.csv", 
                                   col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                    value = col_double(),
                                                    value_last_year = col_double(),
                                                    value_indicator = col_double(),
                                                    value_difference = col_double(),
                                                    prediction = col_double(),
                                                    prediction_indicator = col_double(),
                                                    error_prediction = col_double()))
  
  country_difference_data = read_csv("data/output_openaqData/country_difference_data.csv", 
                                     col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                      value = col_double(),
                                                      value_last_year = col_double(),
                                                      value_indicator = col_double(),
                                                      value_last_year_indicator = col_double(),
                                                      value_difference = col_double(),
                                                      prediction = col_double(),
                                                      prediction_indicator = col_double(),
                                                      error_prediction = col_double()))
  
  country_difference_data_standardized = read_csv("data/output_openaqData/country_difference_data_standardized.csv", 
                                                  col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                                   value = col_double(),
                                                                   value_last_year = col_double(),
                                                                   value_indicator = col_double(),
                                                                   value_last_year_indicator = col_double(),
                                                                   value_difference = col_double(),
                                                                   prediction = col_double(),
                                                                   prediction_indicator = col_double(),
                                                                   error_prediction = col_double()))
  
  subregion_difference_data = read_csv("data/output_openaqData/subregion_difference_data.csv", 
                                       col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                        value = col_double(),
                                                        value_last_year = col_double(),
                                                        value_indicator = col_double(),
                                                        value_last_year_indicator = col_double(),
                                                        value_difference = col_double(),
                                                        prediction = col_double(),
                                                        prediction_indicator = col_double(),
                                                        error_prediction = col_double()))
  
  subregion_difference_data_standardized = read_csv("data/output_openaqData/subregion_difference_data_standardized.csv", 
                                                    col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                                     value = col_double(),
                                                                     value_last_year = col_double(),
                                                                     value_indicator = col_double(),
                                                                     value_last_year_indicator = col_double(),
                                                                     value_difference = col_double(),
                                                                     prediction = col_double(),
                                                                     prediction_indicator = col_double(),
                                                                     error_prediction = col_double()))
  
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
    # region_effect_list = "WESTERN EUROPE"
    country_effect_list = unique(global_mobility_report_clean_stringency_index$CountryCode)
    
    # CHOICE: remove obs with missing values 
    only_data_with_predictions = T
    
    # CHOICE: Data on which level (country or subregion) - country_difference_data, subregion_difference_data, country_difference_data_standardized, subregion_difference_data_standardized
    effect_data_raw = subregion_difference_data
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
    
  #### CHOICE: choose effect variable in first summarize command ###
    # variables: error_prediction, prediction, value_difference, value
    # take means / values at covid
    effect_data = effect_data %>%
      group_by(CountryCode, Region, sub_region_1,parameter, Date, covid_time) %>%
      summarize(StringencyIndex = mean(StringencyIndex, na.rm=T), value = mean(value_difference, na.rm=T)) %>% # summarize over country / day
      group_by(CountryCode, sub_region_1,Region ,parameter, covid_time) %>%
      mutate(StringencyIndex = mean(StringencyIndex, na.rm=T)) %>% # take mean stringency index for before / after covid. value is already MA (from input)
      filter(Date== mid_covid_timeframe | Date == mid_no_covid_timeframe) %>%
      arrange(CountryCode,Region,sub_region_1,parameter,covid_time)
    summary(effect_data)
    
    filter(effect_data, parameter == "no2")
    # Remove observations without values (do if weather model predictions are used)
    if (only_data_with_predictions==T) {
      effect_data = filter(effect_data, is.na(value)==F)
    }
    summary(effect_data)
    # calculate difference in value and covid for plot.
    # CAREFUL: this is not the same as the raw diff / diff approach
      
      effect_data  = effect_data %>%
        group_by(CountryCode, Region , sub_region_1,parameter) %>%
        arrange(CountryCode, parameter, sub_region_1,covid_time) %>%
        mutate(StringencyIndex_difference = StringencyIndex-lag(StringencyIndex),value_difference = value-lag(value))
      
    # remove outliers in stringencyIndex_difference
      effect_data = filter(effect_data, StringencyIndex_difference > 25 )
  
    # investigate difference between before covid and in covid time
    summary(effect_data[effect_data$covid_time==0,])
    summary(effect_data[effect_data$covid_time==1,])
    
    # Effect estimation
    
      # Visual: scatter plot difference in pollution and difference in stringency to investigate effect
      for (i in 1:length(unique(effect_data$parameter))) {
        effect_data_scatter  = filter(effect_data, parameter ==unique(effect_data$parameter)[i])
        # effect_data_scatter =  filter(effect_data_scatter, covid_time==1)
        
        title = paste0("Scatter_estimation", unique(effect_data$parameter)[i])
        print(ggplot(effect_data_scatter, aes(x = StringencyIndex_difference, y = value_difference, col=Region)) +
                geom_point() +
                geom_smooth(method='lm', aes(group=Region)) + 
                geom_smooth(method='lm') + 
                ggtitle(unique(effect_data$parameter)[i])+ 
                ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
        # 
    
    # title = paste0("Scatter_estimation_facet", unique(effect_data$parameter)[i])
    #     print(ggplot(effect_data_scatter, aes(x = StringencyIndex, y = value, col = Region)) +
    #             geom_point() +
    #             facet_wrap(vars(covid_time)) + 
    #             geom_smooth(method='lm', aes(group=Region)) + 
    #             geom_smooth(method='lm') + 
    #             ggtitle(unique(effect_data$parameter)[i])+
    #             ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
      }
  
  
    # Difference in Difference estimation for all pollution parameters
      # choose parameters
      parameters_pollution = unique(effect_data$parameter)
    
      # classic model: value on time, stringency, time * change in stringency (last is causal effect)
      diff_diff_model = list()
      for (i in 1:length(parameters_pollution)) {
        
        model_data = filter(effect_data, parameter == parameters_pollution[i])
        print(parameters_pollution[i])
        model = lm(value ~ covid_time + StringencyIndex+covid_time*StringencyIndex, data = model_data)
        diff_diff_model[[i]] = model
        print(summary(model))
      }
      
      # regress value on time and time * change in stringency 
      diff_diff_model_v2 = list()
      for (i in 1:length(parameters_pollution)) {
        
        model_data = filter(effect_data, parameter == parameters_pollution[i])
        print(parameters_pollution[i])
        
        model_data$interact = model_data$covid_time*model_data$StringencyIndex_difference
        model = lm(value ~ covid_time + interact, data = model_data)
        diff_diff_model_v2[[i]] = model
        print(summary(model))
      }

      # regress change in airpollution on change in stringency
      diff_diff_model_change = list()
      for (i in 1:length(parameters_pollution)) {
        model_data = filter(effect_data, parameter == parameters_pollution[i] & covid_time==1)
        
        print(parameters_pollution)
        model = lm(value_difference ~ StringencyIndex_difference, data = model_data)
        diff_diff_model_change[[i]] = model
        print(summary(model))
      }
      
    # pick final model for effect estimation
    # model_effect = 
      
# Effect prediction
    
  
  
