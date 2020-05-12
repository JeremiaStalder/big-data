# detrend and deseasonalize air pollution data

# librarys
library(tidyverse)
library(readr)
library(zoo)
library(RMariaDB) 
library(stats)
# map librarys
library(sp)
library(rgdal)
library(raster)# might need vector operations on map data


setwd("~/GitHub/big-data") # setwd
source("functions.R") # functions
outpath = "./output/map_visualization/" # output

# Import Data
  # Map Data ----
  map_us = readRDS("data/map_data/gadm36_USA_2_sp.rds")
map_switzerland = readRDS("data/map_data/switzerland_gadm36_CHE_gpkg/gadm36_CHE_2_sp.rds")
  map_switzerland_gpkg = readOGR("data/map_data/switzerland_gadm36_CHE_gpkg/gadm36_CHE.gpkg", verbose = FALSE)

  
  #summary(map_us)
  summary(map_switzerland)
  
  map_us = fortify(map_us)
  map_data(map_switzerland, )
  map_switzerland_fortify = fortify(map_switzerland, region = "NAME_1")
  

  

  ggplot(map_switzerland_fortify, aes(long, lat)) +
    geom_polygon(aes(group = group), colour = "white")
  
  locations <- ggplot(map_switzerland_fortify, aes(x=long, y=lat))
  locations <- locations + geom_map(data = map_switzerland_fortify,
                                    map = map_switzerland_fortify,
                                    aes(map_id = id)) +
    geom_polygon(aes(group = group))
  

  
  
  locations
  by_subregion <- locations + geom_map(data=pop, map=map, color="white", size=0.15,
                                       aes(fill=log(X2013), group=Country.Code, map_id=Country.Code))
  