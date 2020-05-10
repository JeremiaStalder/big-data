# Cleaning + Summary Statistics for Nationwide Data
# Datasets used: 
  # Oxford Coronavirus Government Response Tracker
  # Countries of the world: countries, continent, basic population and economic data

# librarys
library(tidyverse)
library(readr)
library(zoo)

setwd("~/GitHub/big-data") # setwd
source("functions.R") # functions
outpath = "./output/descriptives_stringency/" #output path


### import data ----
government_response_data_original <- read_csv("data/oxford_government_response_tracker/covid_government_response_tracker.csv", 
                                              col_types = cols(Date = col_date(format = "%Y%m%d")))

countries_of_the_world_original <- read_csv("data/countries-of-the-world/countries of the world.csv") # to merge with country variables

wikipedia_iso_country_codes <- read_csv("data/countries-iso-codes/wikipedia-iso-country-codes.csv") # to change iso codes for merging


### datacleaning ----

  # keep only subindex and index variables of goverment ticker data
    varnames = c("CountryName","CountryCode","Date","C1_School closing","C2_Workplace closing","C3_Cancel public events","C4_Restrictions on gatherings",
                 "C5_Close public transport","C6_Stay at home requirements","C7_Restrictions on internal movement","C8_International travel controls",
                 "E1_Income support","E2_Debt/contract relief","E3_Fiscal measures","E4_International support","H1_Public information campaigns",
                 "H2_Testing policy","H3_Contact tracing","H4_Emergency investment in healthcare","H5_Investment in vaccines","M1_Wildcard",
                 "ConfirmedCases","ConfirmedDeaths","StringencyIndex","StringencyIndexForDisplay","LegacyStringencyIndex","LegacyStringencyIndexForDisplay")
    
    government_response_data = government_response_data_original[,varnames]
  
  # take data from Start January to End April
   government_response_data = filter(government_response_data, Date>="2020-01-01" & Date<="2020-04-30")
  
## merge country information to government response data
  
  # identify countries with different spelling and rename
    indicator_change_country_name = sort(match(countries_of_the_world_original$Country, unique(government_response_data_original$CountryName)))
    unique(government_response_data_original$CountryName)[-indicator_change_country_name] # show which countries are not in countries of the world list
    
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Bosnia & Herzegovina","Bosnia and Herzegovina")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Congo, Repub. of the","Democratic Republic of Congo")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Gambia, The","Gambia")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Kyrgystan","Kyrgyz Republic")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Korea, South","South Korea")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Macau","Macao")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Burma","Myanmar")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Slovakia","Slovak Republic")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Swaziland","Eswatini")
    countries_of_the_world_original$Country = replace(countries_of_the_world_original$Country,countries_of_the_world_original$Country=="Trinidad & Tobago","Trinidad and Tobago")
    
  # remove baltics region and include to eastern europe  
    countries_of_the_world_original$Region = replace(countries_of_the_world_original$Region,countries_of_the_world_original$Region=="BALTICS","EASTERN EUROPE") 
 
     # combine datasets 
    # get country data for countries that match
      countries_of_the_world = filter(countries_of_the_world_original, countries_of_the_world_original$Country %in% unique(government_response_data_original$CountryName)) 
      colnames(countries_of_the_world)[1] = "CountryName"
    # Merge Data: Removed countries from government ticker: "Kyrgyz Republic","Palestine","South Sudan",Kosovo"
      merged_data = full_join(government_response_data, countries_of_the_world, by="CountryName")
  
    # change iso alpha 3 to iso alpha 2 country codes
      wikipedia_iso_country_codes =  select(wikipedia_iso_country_codes, "Alpha-3 code", "Alpha-2 code")
      colnames(wikipedia_iso_country_codes) = c("CountryCode", "Alpha_2_code")
      country_iso2 = select(left_join(merged_data, wikipedia_iso_country_codes, by= c("CountryCode")), "Alpha_2_code")
      merged_data[1:nrow(merged_data),c("CountryCode")] = country_iso2 # assign iso2 as CountryCode, Do not change col name 
      
    # change CountryCode from na to "na" for namibia. Remove countries without code (Kosovo, South Sudan)
      merged_data = mutate(merged_data, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) 
      merged_data = merged_data[is.na(merged_data$CountryCode)==F,] # only take countries with country code
      
## Clean Dataset

  # drop countries for which no stringency index is available for any timeframe (sorts out 0 countries)
    countries_with_data = unique(filter(merged_data, is.na(StringencyIndex)==F)$CountryCode)
    merged_data = filter(merged_data, CountryCode %in% countries_with_data)
    
  # drop countries with stringency index of 0 for entire timeframe (sorts out 0 countries)
    countries_with_stringency_above_0 = group_by(merged_data, CountryCode) %>%
      summarize(mean(StringencyIndex, na.rm=T))
    merged_data = filter(merged_data, CountryCode %in% countries_with_stringency_above_0$CountryCode)
    
  # set stringency index to previous value if no data
    # set stringency index to 0 if missing for first observation
    for (i in 1:length(unique(merged_data$CountryName))) {
      data = filter(merged_data, CountryName == unique(merged_data$CountryName)[i])$StringencyIndex
      # replace NaNs
      for (j in 1:length(data)) {
        if((is.nan(data[j]) | is.na(data[j])) & j!=1){
          data[j] = data[j-1]
        } else if ((is.nan(data[j]) | is.na(data[j])) & j==1){ # set first value to 0 if missing
          data[j] = 0
        }
      }
      # reassign to data
      merged_data[merged_data$CountryName==unique(merged_data$CountryName[i]),]$StringencyIndex = data
    }
  
  # save data  
    merged_data_clean  = merged_data
    write.csv(merged_data_clean,file="./data/clean/nationwide_data_clean.csv")


## Summary Statistics & Plots ----
  stringency_time = group_by(merged_data_clean, Date) %>%
    summarize(mean_stringency=mean(StringencyIndex,na.rm=T), median_stringency = median(StringencyIndex, na.rm=T), na_stringency = sum(is.na(StringencyIndex)==1), 
              sum_infected=sum(ConfirmedCases,na.rm=T), sum_death =sum(ConfirmedDeaths, na.rm=T))
  
  print(summary(stringency_time))
  
  line_plot_multiple("Stringency over Time", outpath, stringency_time$Date,"Date", "Stringency Index", names_y=c("mean", "median"), 
                     y_percent=F, legend=T, stringency_time$mean_stringency, stringency_time$median_stringency)
  line_plot_multiple("Total Covid19 Cases over Time",outpath, stringency_time$Date,"Date", "Cases", names_y=c("Infected", "Deaths"), 
                     y_percent=F, legend=T, stringency_time$sum_infected, stringency_time$sum_death)
  
  
  stringency_region = group_by(merged_data_clean, Region, Date) %>%
    summarize(mean_stringency=mean(StringencyIndex,na.rm=T), median_stringency = median(StringencyIndex, na.rm=T), na_stringency = sum(is.na(StringencyIndex)==1), 
              sum_infected=sum(ConfirmedCases,na.rm=T), sum_death =sum(ConfirmedDeaths, na.rm=T))
  
  
  # assign y-variables for plot
    for (i in 1:(length(unique(stringency_region$Region))-1)) { # of regions -1 (na region)
      name = paste0("y",i)
      assign(name,filter(stringency_region, Region == unique(stringency_region$Region)[i])$mean_stringency)
    }
    
    line_plot_multiple("Stringency per Region over Time", outpath,stringency_time$Date,"Date", "Stringency Index", names_y=unique(stringency_region$Region),
                       y_percent=F, legend=T, y1, y2,y3,y4,y5,y6,y7,y8, y9, y10, y11)
    
  # assign y-variables for plot
    # replace NaNs / NA values with previous value. In loop because has to happen sequentially. 
    for (i in 1:(length(unique(stringency_region$Region))-1)) { # of regions -1 (na region)
      name = paste0("y",i)
      assign(name,filter(stringency_region, Region == unique(stringency_region$Region)[i])$sum_infected)
      data = get(name)
      # replace NaNs
      for (j in 1:length(data)) {
        if((is.nan(data[j]) | is.na(data[j])) & j!=1){
          data[j] = data[j-1]
        } else if ((is.nan(data[j]) | is.na(data[j])) & j==1){ # set first value to 0 if missing
          data[j] = 0
            }
          }
          # reassign to data
          assign(name, data)
        }
        
        line_plot_multiple("Covid19 Cases per Region over Time", outpath,stringency_time$Date,"Date", "Cases", names_y=unique(stringency_region$Region), 
                           y_percent=F, legend=T,y1, y2,y3,y4,y5,y6,y7,y8, y9, y10)
        
        
    