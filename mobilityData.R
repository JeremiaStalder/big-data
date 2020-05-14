### cleaning global_mobility_report ----

# librarys
library(tidyverse)
library(readr)
library(zoo)

setwd("~/GitHub/big-data") # setwd
source("functions.R") # functions

### import data ----

global_mobility_report <- read_delim("data/google_mobility/Global_Mobility_Report.csv", 
                                     ";",col_types = cols(date = col_date(format = "%d/%m/%Y")), escape_double = FALSE, trim_ws = TRUE) # load Google Mobility Data

### Prepocessing ----
  # summary 
    summary(global_mobility_report)
  # summary(is.na(global_mobility_report))
  
  # rename cols to match nationwide data
    colnames(global_mobility_report)[1] = "CountryCode" 
    colnames(global_mobility_report)[2] = "CountryName" 
    colnames(global_mobility_report)[5] = "Date" 
  
  # change CountryCode from na to "na" for namibia. Remove countries without code
    global_mobility_report = mutate(global_mobility_report, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode))
    global_mobility_report = global_mobility_report[is.na(global_mobility_report$CountryCode)==F,] # only take countries with country code
    
  # change subregions to lowercase letter
    global_mobility_report$sub_region_1 = tolower(global_mobility_report$sub_region_1)
  
  # take daily means per subregion if multiple obs per subregion
    global_mobility_report = group_by(global_mobility_report, Date, CountryCode, CountryName,sub_region_1) %>%
      summarize(retail_and_recreation_percent_change_from_baseline = mean(retail_and_recreation_percent_change_from_baseline, na.rm=T),
                grocery_and_pharmacy_percent_change_from_baseline = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm=T),
                parks_percent_change_from_baseline = mean(parks_percent_change_from_baseline, na.rm=T),
                transit_stations_percent_change_from_baseline = mean(transit_stations_percent_change_from_baseline, na.rm=T),
                workplaces_percent_change_from_baseline = mean(workplaces_percent_change_from_baseline, na.rm=T),
                residential_percent_change_from_baseline = mean(residential_percent_change_from_baseline, na.rm=T))
    
  # drop observations with NA values for indices
    # drop if X or more indices are NA
    max_number_missing_indices = 6 # 0==all indices necessary, 6== no index necessary
    global_mobility_report = filter(global_mobility_report, (is.na(retail_and_recreation_percent_change_from_baseline) +
                                                               is.na(grocery_and_pharmacy_percent_change_from_baseline) +
                                                               is.na(parks_percent_change_from_baseline) +
                                                               is.na(transit_stations_percent_change_from_baseline) +
                                                               is.na(workplaces_percent_change_from_baseline) +
                                                               is.na(residential_percent_change_from_baseline))<=max_number_missing_indices)
    
  # shorten variable names
    mobility_variables_original = c("retail_and_recreation_percent_change_from_baseline",
                                    "grocery_and_pharmacy_percent_change_from_baseline",
                                    "parks_percent_change_from_baseline",
                                    "transit_stations_percent_change_from_baseline",
                                    "workplaces_percent_change_from_baseline",
                                    "residential_percent_change_from_baseline")
    mobility_variables = sapply(strsplit(mobility_variables_original,split="_percent_change_from_baseline"), function(x) (x[1]))
    colnames(global_mobility_report)[colnames(global_mobility_report) %in% mobility_variables_original] = mobility_variables
  
  # save cleaned dataset
    global_mobility_report_clean  = global_mobility_report
    
    con = file("./data/clean/global_mobility_report_clean.csv","w", encoding="utf-8")
     write.csv(global_mobility_report_clean, con, row.names=FALSE)
    close(con)
  
