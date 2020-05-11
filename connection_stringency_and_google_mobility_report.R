# Investigate Relationship Oxford Covid Tracker & Google Mobility Data

# librarys
library(tidyverse)
library(readr)
library(zoo)
library(caret) # random forests
library(grf)

setwd("~/GitHub/big-data") # setwd
source("functions.R") # functions

# output
outpath = "./output/stringency_and_mobility/"


### import data ----
  nationwide_data_clean <- read_csv("data/clean/nationwide_data_clean.csv") # load nationwide data
  global_mobility_report_clean <- read_csv("data/clean/global_mobility_report_clean.csv", 
                                           col_types = cols(Date = col_date(format = "%Y-%m-%d"))) # load mobility data
  # replace NA with "NA" country code
   global_mobility_report_clean = mutate(global_mobility_report_clean, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) # redo because "NA" is imported as NA    
   nationwide_data_clean = mutate(nationwide_data_clean, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) #redo because "NA" is imported as NA
      
  # define key mobility variables 
   mobility_variables_original = c("retail_and_recreation_percent_change_from_baseline",
                                   "grocery_and_pharmacy_percent_change_from_baseline",
                                   "parks_percent_change_from_baseline",
                                   "transit_stations_percent_change_from_baseline",
                                   "workplaces_percent_change_from_baseline",
                                   "residential_percent_change_from_baseline")
   mobility_variables = sapply(strsplit(mobility_variables_original,split="_percent_change_from_baseline"), function(x) (x[1]))
   
                                                                     
###  merge data (country level) ----
  
   # right join by CountryCode and Date. Remove obs where one of the datapoints is not given. Remove CountryName of nationwide data. Take 
   merged_data = inner_join(select(global_mobility_report_clean, -X1), select(nationwide_data_clean,-c("CountryName", "X1")), by = c("CountryCode", "Date")) 

    summary(merged_data) # dataset merged by country and data
    
  # working datasets
    merged_data_countries = filter(merged_data, is.na(sub_region_1))  
    
### Descriptives ----

    
    regions = unique(merged_data_countries$Region)[is.na(unique(merged_data_countries$Region))==F] # exclude countries without region
    
    # summary 
    summary(is.na(merged_data_countries))
    summary(merged_data_countries)
    
    
   # Correlations
    # Worldwide
    print("Strong Negative Correlation of Stringency and Mobility Variables - overall Cross Section")
    print("Grocery/Pharmacy and Parks lower correlation than others")
    print("Residential strong positive correlation (obviously)")
    cor(merged_data_countries[,c("StringencyIndex", mobility_variables)] ,use = "pairwise.complete.obs")
    
    # In Continents/Regions
    print("Correlation of Stringency and Mobility Variables - Regions")
    print("Similar for all regions. Europe and Baltics have lower sensitivity of park measure.")
    for (i in 1:length(regions)) {
      data = merged_data_countries[merged_data_countries$Region == regions[i],c("StringencyIndex", mobility_variables)]
      print(cor(data ,use = "pairwise.complete.obs"))
    }
    
    # Plot 
    # One country
    # get data
    stringency_mobility_ww_av = group_by(merged_data_countries, sub_region_1) %>%
      filter(CountryCode =="DE") %>%
      arrange(Date) %>%
      mutate(StringencyIndex= rollapply(StringencyIndex, width = 7, mean,na.rm=T, fill=lag(StringencyIndex),by=1), 
             retail_and_recreation = rollapply(retail_and_recreation, width = 7, mean,na.rm=T, fill=retail_and_recreation), 
             grocery_and_pharmacy =  rollapply(grocery_and_pharmacy, width = 7, mean,na.rm=T, fill=grocery_and_pharmacy), 
             parks = rollapply(parks, width = 7, mean,na.rm=T, fill=parks), 
             transit_stations = rollapply(transit_stations, width = 7, mean,na.rm=T, fill=transit_stations), 
             workplaces  =rollapply(workplaces, width = 7, mean,na.rm=T, fill=workplaces), 
             residential=  rollapply(residential, width = 7, mean,na.rm=T, fill=residential)) %>%
      group_by(Date)%>%
      summarize(StringencyIndex = mean(StringencyIndex,na.rm=T), 
                retail_and_recreation=  mean(retail_and_recreation, na.rm=T), 
                grocery_and_pharmacy=  mean(grocery_and_pharmacy, na.rm=T), 
                parks=  mean(parks, na.rm=T), 
                transit_stations=  mean(transit_stations, na.rm=T), 
                workplaces=  mean(workplaces, na.rm=T), 
                residential=  mean(residential, na.rm=T))
    
    # get plot variables
    y1 = stringency_mobility_ww_av$StringencyIndex
    
    for (i in 1:(length(mobility_variables))) { # mobility vars + 1(stringency) +1(infected)
      name = paste0("y",i+2)
      data = select(stringency_mobility_ww_av, mobility_variables[i])
      colnames(data) = "var"
      assign(name, -data$var)
    }
    
    line_plot_multiple("Stringency and Mobility simple averages - DE", outpath, stringency_mobility_ww_av$Date,"Date", "Stringency and Mobility (Mobility Change *-1)", names_y=c("Stringency Index",  mobility_variables),
                       y_percent=F, legend=T, y1,y3,y4,y5,y6,y7,y8)    
    
  # Plot 
    # Worldwide simple averages
      # get data
      stringency_mobility_ww_av = group_by(merged_data_countries, Date) %>%
        summarize(StringencyIndex = mean(StringencyIndex,na.rm=T), 
                  retail_and_recreation=  mean(retail_and_recreation, na.rm=T), 
                  grocery_and_pharmacy=  mean(grocery_and_pharmacy, na.rm=T), 
                  parks=  mean(parks, na.rm=T), 
                  transit_stations=  mean(transit_stations, na.rm=T), 
                  workplaces=  mean(workplaces, na.rm=T), 
                  residential=  mean(residential, na.rm=T), 
                  ConfirmedCases=sum(ConfirmedCases,na.rm=T))
      
      # get plot variables
      y1 = stringency_mobility_ww_av$StringencyIndex
      y2 = log(stringency_mobility_ww_av$ConfirmedCases)*5 # log case number time random scalar for better depiction
      
      for (i in 1:(length(mobility_variables))) { # mobility vars + 1(stringency) +1(infected)
        name = paste0("y",i+2)
        data = select(stringency_mobility_ww_av, mobility_variables[i])
        colnames(data) = "var"
        assign(name, -data$var)
      }
      
      line_plot_multiple("Stringency and Mobility simple averages - Worldwide", outpath, stringency_mobility_ww_av$Date,"Date", "Stringency and Mobility (Mobility Change *-1)", names_y=c("Stringency Index", "log(ConfirmedCases)*5(to see better)", mobility_variables),
                         y_percent=F, legend=T, y1, y2,y3,y4,y5,y6,y7,y8)
      
    # Worldwide population weighed averages
      # get data
      stringency_mobility_ww_pop_average = group_by(merged_data_countries, Date) %>%
        summarize(StringencyIndex = weighted.mean(StringencyIndex,dplyr::coalesce(Population,0),na.rm=T), 
                  retail_and_recreation= weighted.mean(retail_and_recreation,dplyr::coalesce(Population,0),na.rm=T), 
                  grocery_and_pharmacy=  weighted.mean(grocery_and_pharmacy,dplyr::coalesce(Population,0),na.rm=T), 
                  parks=  weighted.mean(parks,dplyr::coalesce(Population,0),na.rm=T), 
                  transit_stations=  weighted.mean(transit_stations,dplyr::coalesce(Population,0),na.rm=T), 
                  workplaces=  weighted.mean(workplaces,dplyr::coalesce(Population,0),na.rm=T), 
                  residential=  weighted.mean(residential,dplyr::coalesce(Population,0),na.rm=T), 
                  ConfirmedCases=sum(ConfirmedCases,na.rm=T))
      
      # get plot variables
      y1 = stringency_mobility_ww_pop_average$StringencyIndex
      y2 = log(stringency_mobility_ww_pop_average$ConfirmedCases)*5 # log case number time random scalar for better depiction
      
      for (i in 1:(length(mobility_variables))) { # mobility vars + 1(stringency) +1(infected)
        name = paste0("y",i+2)
        data = select(stringency_mobility_ww_pop_average, mobility_variables[i])
        colnames(data) = "var"
        assign(name, -data$var)
      }
      
      line_plot_multiple("Stringency and Mobility population-weighed Averages - Worldwide", outpath, stringency_mobility_ww_pop_average$Date,"Date", "Stringency and Mobility (Mobility Change *-1)", names_y=c("Stringency Index", "log(ConfirmedCases)*5(to see better)", mobility_variables),
                         y_percent=F, legend=T, y1, y2,y3,y4,y5,y6,y7,y8)
      
    # Region population weighed averages
      
      for (i in 1:length(regions)) {
        # get data
        stringency_mobility_region_pop_average = filter(merged_data_countries, Region==regions[i]) %>%
          group_by(Date) %>%
          summarize(StringencyIndex = weighted.mean(StringencyIndex,dplyr::coalesce(Population,0),na.rm=T), 
                    retail_and_recreation= weighted.mean(retail_and_recreation,dplyr::coalesce(Population,0),na.rm=T), 
                    grocery_and_pharmacy=  weighted.mean(grocery_and_pharmacy,dplyr::coalesce(Population,0),na.rm=T), 
                    parks=  weighted.mean(parks,dplyr::coalesce(Population,0),na.rm=T), 
                    transit_stations=  weighted.mean(transit_stations,dplyr::coalesce(Population,0),na.rm=T), 
                    workplaces=  weighted.mean(workplaces,dplyr::coalesce(Population,0),na.rm=T), 
                    residential=  weighted.mean(residential,dplyr::coalesce(Population,0),na.rm=T), 
                    ConfirmedCases=sum(ConfirmedCases,na.rm=T))
        
        # get plot variables
        y1 = stringency_mobility_region_pop_average$StringencyIndex
        y2 = log(stringency_mobility_region_pop_average$ConfirmedCases)*5 # log case number time random scalar for better depiction
        
        for (j in 1:(length(mobility_variables))) { # mobility vars + 1(stringency) +1(infected)
          name = paste0("y",j+2)
          data = select(stringency_mobility_region_pop_average, mobility_variables[j])
          colnames(data) = "var"
          assign(name, -data$var)
        }
        
        print(line_plot_multiple(paste("Stringency and Mobility population-weighed Averages -", regions[i]), outpath, stringency_mobility_region_pop_average$Date,"Date", "Stringency and Mobility (Mobility Change *-1)", names_y=c("Stringency Index", "log(ConfirmedCases)*5(to see better)", mobility_variables),
                           y_percent=F, legend=T, y1, y2,y3,y4,y5,y6,y7,y8))
      }
   
      
    
### Model Stringency Index ----   
  # Model stringency index to get proxy for stringency per country subregion?
  # desired output: predicted stringency index per country subregion, for countries without subregions use reported stringency index
    # simple
    # with region interaction effect (europe and baltics seem to have almost no park effect)
      
  #OLS - hard to use due to missing values
    linear_model = lm(StringencyIndex ~ . , data = merged_data_countries[,c("StringencyIndex", mobility_variables)])
    summary(linear_model)
    
    # get prediction
    predicted_index = predict.lm(linear_model, global_mobility_report_clean[,mobility_variables])
    # problem: bounded outcome variable
    # problem: missing values
    summary(predicted_index)
    
    
  # Random forest
    # specific regions.
    region_effect_list = unique(merged_data_countries$Region)
    
    # variables for estimation
      # mobility variabes and numeric country characteristics 
    
      # add numeric region + numeric date to data. 
        # first testset of variables
          merged_data_countries_forest = merged_data_countries
          merged_data_countries_forest$Date_numeric = as.numeric(merged_data_countries_forest$Date)
          
          # one hot encoding for region
          for (i in 1:length(unique(merged_data_countries_forest$Region))) {
            region_dummy = ifelse(merged_data_countries_forest$Region == unique(merged_data_countries_forest$Region)[i],1,0)
            merged_data_countries_forest = cbind(merged_data_countries_forest, region_dummy)
            colnames(merged_data_countries_forest)[ncol(merged_data_countries_forest)] = paste0("Region_",i,"_numeric")
          }
          
          region_numeric = paste0("Region_",seq(1,length(unique(merged_data_countries_forest$Region))),"_numeric")
          country_characteristics = c("ConfirmedCases" ,"ConfirmedCases","Population", "Area (sq. mi.)", 
                                      "Infant mortality (per 1000 births)", "GDP ($ per capita)",
                                      "Literacy (%)", "Phones (per 1000)",  "Other (%)", "Birthrate" ,"Deathrate")
          # forest_covariates = c("Date_numeric",region_numeric,mobility_variables, country_characteristics)
          
        # final set of variables: only use mobility data on subregion level and scale with observed country level. other covarites apart from time had low variable importance
          forest_covariates = c("Date_numeric",mobility_variables)
      
    # data for estimation: take MA for stringency index due to potential "adaptation time", MA for others due to "seasonalities"
      # use march / april data to fit this model
      # apply specific region filter
      length_rolling_window_stringency_index = 1
      length_rolling_window_mobility_variables = 1
    
      model_estimation_data = merged_data_countries_forest %>%
        group_by(CountryCode) %>%
        arrange(Date) %>%
        mutate(StringencyIndex= rollapply(StringencyIndex, width = length_rolling_window_stringency_index, mean,na.rm=T, fill=StringencyIndex,by=1), 
               retail_and_recreation = rollapply(retail_and_recreation, width = length_rolling_window_mobility_variables, mean,na.rm=T, fill=retail_and_recreation), 
               grocery_and_pharmacy =  rollapply(grocery_and_pharmacy, width = length_rolling_window_mobility_variables, mean,na.rm=T, fill=grocery_and_pharmacy), 
               parks = rollapply(parks, width = length_rolling_window_mobility_variables, mean,na.rm=T, fill=parks), 
               transit_stations = rollapply(transit_stations, width = length_rolling_window_mobility_variables, mean,na.rm=T, fill=transit_stations), 
               workplaces  =rollapply(workplaces, width = length_rolling_window_mobility_variables, mean,na.rm=T, fill=workplaces), 
               residential=  rollapply(residential, width = length_rolling_window_mobility_variables, mean,na.rm=T, fill=residential))
      
      model_estimation_data = filter(model_estimation_data, (Region %in% region_effect_list) & (Date >='2020-02-15') & (Date <='2020-04-19'))
    
        # split in train and test
        set.seed(1)
          share_train = 0.8
          train_index <- createDataPartition(model_estimation_data$Region, p = share_train, 
                                                     list = FALSE, 
                                                     times = 1) #balanced split wrt training
        
          train <- model_estimation_data[train_index,]
          test <- model_estimation_data[-train_index,]
        
        forest_model_evaluation <- regression_forest(train[,forest_covariates],train$StringencyIndex)
        
        # "predict" on train -> also important since good in sample fit is good to fit subregions of countries
        fit <- predict(forest_model_evaluation, newdata = train[,forest_covariates])$predictions
        summary(fit-train$StringencyIndex)
        
        r_squared_forest <- r_sq(train$StringencyIndex, fit)
        r_squared_forest
        forest_model_evaluation
        
        # Prediction on test
        fit <- predict(forest_model_evaluation, newdata = test[,forest_covariates])$predictions
        summary(fit-test$StringencyIndex)
        
        r_squared_forest <- r_sq(test$StringencyIndex, fit)
        r_squared_forest
        forest_model_evaluation
      
        test$fit = fit
        data = group_by(test, Date) %>%
          summarize(StringencyIndex = mean(StringencyIndex, na.rm=T), sd = sd((fit-StringencyIndex), na.rm=T), fit = mean(fit, na.rm=T))
        
        line_plot_multiple(paste("Test Forest"), outpath, data$Date,"Date", "Stringency and Prediced", names_y=c("Stringency Index Predicted", "Stringency Index True"),
                                 y_percent=F, legend=T, data$fit , data$StringencyIndex, data$sd)
        
        country_data_predicted = select(test, fit, StringencyIndex, CountryCode,Date) %>%
          mutate(prediction_error = fit-StringencyIndex)
        
    # final estimation and prediction
        # estimate model using all data
        forest_final <- regression_forest(model_estimation_data[,forest_covariates],model_estimation_data$StringencyIndex)
        
        # predict stringency index for subregions
          # add necessary covariates to data
          merged_data_with_prediction = merged_data
          merged_data_with_prediction$Date_numeric = as.numeric(merged_data_with_prediction$Date)
        
          # predict using forest
          merged_data_with_prediction$StringencyIndexPredicted = predict(forest_final, newdata = merged_data_with_prediction[,forest_covariates])$predictions
          
         
          
          # scale daily predicted index of subregions to match observed country index. Col StringencyIndex is country Stringency Index
          merged_data_with_prediction_with_countrymeans = group_by(merged_data_with_prediction, CountryCode, Date) %>%
            mutate(country_indicator = ifelse(is.na(sub_region_1)==T, 1,0)) %>% #differentiate between regions and countries
            mutate(country_scaling_factor = ifelse(country_indicator==1,StringencyIndex / StringencyIndexPredicted, 0)) %>%
            group_by(CountryCode, Date, country_indicator) %>%
            mutate(country_scaling_factor = ifelse(country_indicator==0,StringencyIndex / mean(StringencyIndexPredicted), country_scaling_factor)) %>%
            mutate(StringencyIndexFinalPrediction = StringencyIndexPredicted * country_scaling_factor)
        
        # test if predictions are plausible
          data_test =merged_data_with_prediction_with_countrymeans[merged_data_with_prediction_with_countrymeans$CountryCode == "AE",
                                                                   c("Date", "CountryCode","sub_region_1", "StringencyIndex", "StringencyIndexPredicted", 
                                                                     "country_indicator","StringencyIndexFinalPrediction")]
        
        # Plot 
        # One country
        # get data
        stringency_model_plot = group_by(merged_data_with_prediction_with_countrymeans, CountryCode,sub_region_1, Date) %>%
          filter(CountryCode=="IT") %>%
          summarize(StringencyIndexPredicted = mean(StringencyIndexFinalPrediction,na.rm=T))
          
        title=paste("Stringency Model - IT")
        print(ggplot(stringency_model_plot, aes(x = Date))+
                geom_line(aes(y=StringencyIndexPredicted, color = sub_region_1))+
                ggtitle(paste(title,sep=" ")) +
                theme(plot.title = element_text(size=10, face="bold"))+
                theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10,face="bold"))+
                ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600))
        
      # remove unnecessary variables
        merged_data_with_prediction_with_countrymeans_final = mutate(merged_data_with_prediction_with_countrymeans, StringencyIndex = StringencyIndexFinalPrediction) %>%
          ungroup() %>%
          select(-c("country_indicator","country_scaling_factor","StringencyIndexFinalPrediction","StringencyIndexPredicted","Date_numeric"))
        
      # save predictions
        write.csv(merged_data_with_prediction_with_countrymeans,file="./data/clean/global_mobility_report_clean_with_predictions_stringency_index.csv")
  