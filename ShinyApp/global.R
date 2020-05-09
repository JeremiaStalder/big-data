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
#source("utils/clean_uber_data.R")
#source("utils/setup.R")
