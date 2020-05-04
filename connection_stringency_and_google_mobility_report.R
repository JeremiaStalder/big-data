# Investigate Relationship Oxford Covid Tracker & Google Mobility Data

# librarys
library(tidyverse)
library(readr)
library(zoo)

### import data ----
nationwide_data_clean <- read_csv("data/clean/nationwide_data_clean.csv") # load nationwide data
global_mobility_report <- read_delim("data/google_mobility/Global_Mobility_Report.csv", 
                                     ";",col_types = cols(date = col_date(format = "%d/%m/%Y")), escape_double = FALSE, trim_ws = TRUE) # load Google Mobility Data

# cleaning global_mobility_report
  # drop NA country codes.
  # drop NA values for indices


# merge data (country level)
colnames(global_mobility_report)[1] = "CountryCode" # get same name for merging variable
colnames(global_mobility_report)[5] = "Date" # get same name for merging variable
merged_data = left_join(global_mobility_report, nationwide_data_clean, by = c("CountryCode", "Date"))

nationwide_data_clean[,c("CountryCode", "Date")][duplicated(nationwide_data_clean[,c("CountryCode", "Date")]),]

