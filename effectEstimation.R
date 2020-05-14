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
outpath = "output/effectEstimation/" # output

# parameters
  # rolling average for value to figure out seasonal patterns. 
  parms_ma = list(15, 30, 60, 12, 20, 45)
  names(parms_ma) = c("short","medium","long","max_na_short","max_na_medium","max_na_long")

# Import Data ----
  country_difference_data = read_csv("data/output_openaqData/country_difference_data.csv", 
                                     col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                      CountryCode = col_character(), 
                                                      Region = col_character(), 
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
                                                                   CountryCode = col_character(), 
                                                                   Region = col_character(), 
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
                                                        Region = col_character(), 
                                                        CountryCode = col_character(), 
                                                        sub_region_1 = col_character(),
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
                                                                     Region = col_character(), 
                                                                     CountryCode = col_character(), 
                                                                     sub_region_1 = col_character(),
                                                                     value = col_double(),
                                                                     value_last_year = col_double(),
                                                                     value_indicator = col_double(),
                                                                     value_last_year_indicator = col_double(),
                                                                     value_difference = col_double(),
                                                                     prediction = col_double(),
                                                                     prediction_indicator = col_double(),
                                                                     error_prediction = col_double()))
  
  global_mobility_report_clean_stringency_index = read_csv("data/output_openaqData/global_mobility_report_clean_to_merge_with_openaq.csv", 
                                                           col_types = cols(CountryName = col_character(), 
                                                                            Date = col_date(format = "%Y-%m-%d"), 
                                                                            Region = col_character(), 
                                                                            CountryCode = col_character(), 
                                                                            sub_region_1 = col_character(),
                                                                            grocery_and_pharmacy = col_double(), 
                                                                            parks = col_double(), residential = col_double(), 
                                                                            retail_and_recreation = col_double(), 
                                                                            sub_region_1 = col_character(), transit_stations = col_double(), 
                                                                            workplaces = col_double()))
  
# Effect Estimation ----

  # Settings for effect estimation:
  
    # CHOICE: analysis variable: airpollution value, weather model prediction - value, value - past years value
    analysis_variable_list = c("value","error_prediction", "value_difference")
    analysis_variable = analysis_variable_list[3]
    
    # CHOICE: save new regressions and update result table for presentation?
    update_result = T # make sure all tables for analysis_variable_list results exist in directory aber running estimimation. Change Analysis var to cover all
  
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
    
    # CHOICE: Missing values
    only_data_with_predictions = T
    min_number_obs_per_model = 5
    
    # CHOICE: Data on which level (country or subregion) - country_difference_data, subregion_difference_data, country_difference_data_standardized, subregion_difference_data_standardized
    effect_data_raw = subregion_difference_data
  
  # Get data for effect estimation
    # Merge with mobility data
    merged_data_effect = inner_join(global_mobility_report_clean_stringency_index, select(effect_data_raw, -c("CountryCode", "Region")), by = c("sub_region_1","Date")) 
    
    # cut timeframe
    effect_data = filter(merged_data_effect, (Date >=start_covid_timeframe & Date <= end_covid_timeframe)| (Date >=start_no_covid_timeframe & Date <= end_no_covid_timeframe)) %>%
      mutate(covid_time = ifelse((Date >=start_covid_timeframe & Date <= end_covid_timeframe), 1,0)) %>%
      filter(Region %in% region_effect_list) %>%
      filter(CountryCode %in% country_effect_list) %>%
      group_by(CountryCode, Region, sub_region_1,parameter, Date, covid_time)
    
    # remove NA region
    effect_data = filter(effect_data, is.na(Region)==F)
    
    # analysis variable_ summarize wrt variable
      if(analysis_variable=="value") {
        effect_data =  summarize(effect_data, StringencyIndex = mean(StringencyIndex, na.rm=T), value = mean(value, na.rm=T))
      } else if(analysis_variable=="error_prediction") {
        effect_data =  summarize(effect_data, StringencyIndex = mean(StringencyIndex, na.rm=T), value = mean(error_prediction, na.rm=T))
      } else if(analysis_variable=="value_difference") {
        effect_data =  summarize(effect_data, StringencyIndex = mean(StringencyIndex, na.rm=T), value = mean(value_difference, na.rm=T))
      }
    
    # take means / values at covid
    effect_data = effect_data %>%
      group_by(CountryCode, sub_region_1,Region ,parameter, covid_time) %>%
      mutate(StringencyIndex = mean(StringencyIndex, na.rm=T)) %>% # take mean stringency index for before / after covid. value is already MA (from input)
      filter(Date== mid_covid_timeframe | Date == mid_no_covid_timeframe) %>%
      arrange(CountryCode,Region,sub_region_1,parameter,covid_time)
    summary(effect_data)
    
    # Remove observations without values (do if weather model predictions are used)
    if (only_data_with_predictions==T) {
      effect_data = filter(effect_data, is.na(value)==F)
    }
    

  ### Difference in Difference Estimation

      # take first differences
      effect_data  = effect_data %>%
        group_by(CountryCode, Region , sub_region_1,parameter) %>%
        arrange(CountryCode, parameter, sub_region_1,covid_time) %>%
        mutate(StringencyIndex_difference = StringencyIndex-lag(StringencyIndex),value_difference = value-lag(value))
      
      # investigate difference between before covid and in covid time
      summary(effect_data[effect_data$covid_time==0,])
      summary(effect_data[effect_data$covid_time==1,])
      
      # remove outliers in stringencyIndex_difference, keep NAs
      effect_data = filter(effect_data, (StringencyIndex_difference > 25 & StringencyIndex_difference < 100) | is.na(StringencyIndex_difference) )
      
      # remove outliers in change airpollution for so2
      if(analysis_variable=="value_difference" | analysis_variable=="value" ) {
        effect_data = mutate(effect_data, value_difference = ifelse((parameter =="so2" & abs(value_difference)>10), NA, value_difference))
      }

      # A) first difference approach: only use complete pairs as observations
      effect_data_first_diff  = effect_data %>%
        filter(covid_time==1 & is.na(StringencyIndex_difference)==F)
      print(paste("First Differences: ", nrow(effect_data_first_diff), "Complete Pairs"))
      
      # B) Pooled OLS. use effect data. depreciated due to 
      print(paste("Pooled OLS: ", nrow(effect_data[effect_data$covid_time==0,]),"before and",nrow(effect_data[effect_data$covid_time==1,]), "in Covid"))
    
    
  # A) Visual Effect: scatter plot Linear Models - First Difference Approach
      for (i in 1:length(unique(effect_data_first_diff$parameter))) {
        effect_data_scatter  = filter(effect_data_first_diff, parameter ==unique(effect_data$parameter)[i])
        
        name = paste0("Scatter_estimation", unique(effect_data$parameter)[i], "_", analysis_variable)
        title = paste0( "Covid-Effect on ",toupper(unique(effect_data$parameter)[i]), " - measured with ", analysis_variable)
         y_label = c("Change Airpollution", "Difference to Estimated Airpollution", "Change Airpollution Difference to previous Year") # label dependent on analysis var
        
        print(ggplot(effect_data_scatter, aes(x = StringencyIndex_difference, y = value_difference, col=Region)) +
                geom_point() +
                xlab(label = "Change in Stringency Index") + 
                ylab(label = y_label[analysis_variable_list==analysis_variable]) + 
                theme_bw() +
                geom_smooth(method='lm', aes(group=Region)) + 
                ggtitle(title)+ 
                ggsave(file=paste0(outpath,name,".png"), width=6, height=4, dpi=600))
      }
  
  
  # Model Estimation
      # estimate model for each region
      # A) First Difference
      effect_per_region_diff = data.frame(region=character(),parameter=character(),coefficient_10=numeric(),p_value=numeric(), stringsAsFactors = F)
      effect_per_subregion_diff = data.frame(region=character(),parameter=character(),subregion=character(),effect=numeric(),stringsAsFactors = F)
      names_effect_per_region_diff = c("region","parameter","coefficient_10","p_value")
      names_effect_per_subregion_diff = c("region","parameter","subregion","effect")
      
      
      for (i in 1:length(unique(effect_data_first_diff$Region))) {
        effect_data_diff_loop = filter(effect_data_first_diff, Region == unique(effect_data_first_diff$Region)[i])
        
        # set parameter list
        parameters_pollution_diff = unique(effect_data_diff_loop$parameter)
        
        # loop though parameters
        for (j in 1:length(parameters_pollution_diff)) {
          # get model data for parameter
          model_data = filter(effect_data_diff_loop, parameter == parameters_pollution_diff[j])

          # estimate model only if number of observations is reached
          if(nrow(model_data) >= min_number_obs_per_model) {
            
            # print(model_data)
            # print(unique(effect_data_first_diff$Region)[i])
            # print(parameters_pollution_diff[j])
            model = lm(value_difference ~ StringencyIndex_difference, data = model_data)
            # print(summary(model))
            
            # predict effect for each subregion relative to a country with mean stringency of region
            estimated_effect = as.numeric(model_data$StringencyIndex_difference-mean(model_data$StringencyIndex_difference)*summary(model)$coefficients[2,1])
            
            # save coefficient and significance for region and parameter
            region_effect = c(as.character(unique(effect_data_first_diff$Region)[i]),
                              as.character(parameters_pollution_diff[j]),
                              as.numeric(summary(model)$coefficients[2,1])*10,
                              as.numeric(summary(model)$coefficients[2,4]))
            
            effect_per_region_diff[nrow(effect_per_region_diff)+1,] =  region_effect

            
            # save effect for each subregion
            subregion_data = data.frame(as.character(unique(effect_data_first_diff$Region)[i]),
                                                 as.character(parameters_pollution_diff[j]),
                                                 as.character(model_data$sub_region_1),
                                                 as.numeric(estimated_effect), stringsAsFactors = F)
            colnames(subregion_data) = names_effect_per_subregion_diff
            
            effect_per_subregion_diff = bind_rows(effect_per_subregion_diff, subregion_data)
            # print(nrow(subregion_data))
            # print(subregion_data)
            # print(length((nrow(effect_per_subregion_diff)+1):(nrow(effect_per_subregion_diff)+nrow(subregion_data))))
            # 
            # effect_per_subregion_diff[(nrow(effect_per_subregion_diff)+1):(nrow(effect_per_subregion_diff)+nrow(subregion_data)),] =  subregion_data
            
          }
        }
      }
      
      # change col-types 
      effect_per_region_diff$coefficient_10 = as.numeric(effect_per_region_diff$coefficient_10)
      effect_per_region_diff$p_value = as.numeric(effect_per_region_diff$p_value)
      
      # add analysis variable to colnames
      names_effect_per_region_diff_with_analysis_var = c("region","parameter",paste0(analysis_variable,c("_coefficient_10","_p_value")))
      names_effect_per_subregion_diff_with_analysis_var = c("region","parameter","subregion",paste0(analysis_variable, "_effect"))
      colnames(effect_per_region_diff) = names_effect_per_region_diff_with_analysis_var
      colnames(effect_per_subregion_diff) = names_effect_per_subregion_diff_with_analysis_var
      
      # summary
      summary(effect_per_region_diff)
      summary(effect_per_subregion_diff)
      
      # Depreciated: Pooled OLS
        # choose parameters
        # parameters_pollution = unique(effect_data$parameter)
        # B) Pooled OLS
        # did_pooled_OLS = list()
        # for (i in 1:length(parameters_pollution)) {
        #   
        #   model_data = filter(effect_data, parameter == parameters_pollution[i])
        #   print(parameters_pollution[i])
        #   model = lm(value ~ covid_time + StringencyIndex+covid_time*StringencyIndex, data = model_data)
        #   did_pooled_OLS[[i]] = model
        #   print(summary(model))
        # }
      

      
  # save estimated effects dataframe 
      write.csv(effect_per_region_diff,file=paste0(outpath,"effect_per_region_diff_",analysis_variable,".csv"), row.names=FALSE)
      write.csv(effect_per_subregion_diff,file=paste0(outpath,"effect_per_subregion_diff_",analysis_variable,".csv"), row.names=FALSE)
      
  # load and combine effect data
      if(update_result==T) {
        # load region effect coefficients

        region_value = read_csv(paste0(outpath, "effect_per_region_diff_value.csv"), 
                 col_types = cols(region =  col_character(),
                                  parameter = col_character(),
                                  value_coefficient_10=col_double(),
                                  value_p_value = col_double()))
        
        region_error_prediction = read_csv(paste0(outpath, "effect_per_region_diff_error_prediction.csv"), 
                 col_types = cols(region =  col_character(),
                                  parameter = col_character(),
                                  error_prediction_coefficient_10=col_double(),
                                  error_prediction_p_value = col_double()))
        
        region_value_difference = read_csv(paste0(outpath, "effect_per_region_diff_value_difference.csv"), 
                 col_types = cols(region =  col_character(),
                                  parameter = col_character(),
                                  value_difference_coefficient_10=col_double(),
                                  value_difference_p_value = col_double()))
        
        # load subregion estimated effects
        subregion_value = read_csv(paste0(outpath, "effect_per_subregion_diff_value.csv"), 
                 col_types = cols(region =  col_character(),
                                  subregion = col_character(),
                                  parameter=col_character(),
                                  value_effect = col_double()))
        
        subregion_error_prediction = read_csv(paste0(outpath, "effect_per_subregion_diff_error_prediction.csv"), 
                col_types = cols(region =  col_character(),
                                 subregion = col_character(),
                                 parameter=col_character(),
                                 error_prediction_effect = col_double()))

        subregion_value_difference = read_csv(paste0(outpath, "effect_per_subregion_diff_value_difference.csv"), 
                col_types = cols(region =  col_character(),
                                 subregion = col_character(),
                                 parameter=col_character(),
                                 value_difference_effect = col_double()))
        
      # merge to one dataset
        # region effects
        region_effects = full_join(region_value, region_error_prediction, by = c("region", "parameter"))
        region_effects = full_join(region_effects, region_value_difference, by = c("region", "parameter"))
        
        # subregion effects
        subregion_effects = full_join(subregion_value, subregion_error_prediction, by = c("region", "parameter", "subregion"))
        subregion_effects = full_join(subregion_effects, subregion_value_difference, by = c("region", "parameter", "subregion"))
        
      # save merged datasets
        write.csv(region_effects,file=paste0(outpath,"region_effects.csv"), row.names=FALSE)
        write.csv(subregion_effects,file=paste0(outpath,"subregion_effects.csv"), row.names=FALSE)
        
        
      }

