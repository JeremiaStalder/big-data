###############################################################################
# Entrypoint for the shiny app
#
# Author: Fabian Karst
# Created 2020-05-20
###############################################################################


# Dependencies ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(janitor)
library(highcharter)
library(lubridate)
library(leaflet)
library(shinycssloaders)
library(sp)
library(shinymaterial)

#  Clean Scripts ----------------------------------------------------------
# laod data before app
#setwd("~/GitHub/big-data") # setwd
#inpath = "presentation/tables_map/"

# Stringency, Pollution, Prediction data over time
all_data_over_time <- read_csv("all_data_over_time.csv",
                               col_types = cols(Date = col_date(format = "%Y-%m-%d")))
all_data_over_time = arrange(all_data_over_time, CountryName, Date)

# # Weather Model Predictions
germany_pollution <- read_csv("germany_rolling.csv",
                              col_types = cols(date = col_date(format = "%Y-%m-%d")))

