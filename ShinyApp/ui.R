###############################################################################
# Entrypoint for the shiny app
#
# Author Fabian Karst, Erik Senn, Jeremia Stadler
# Created 2020-05-20
###############################################################################
library(readr)
library(zoo)
library(stats)
library(forecast)
library(lubridate)
library(data.table)
library(tidyverse)

ui = shiny::htmlTemplate(
  # Index Page
  "www/index2.html",
  
  # Topic
  ## title
  page1_title = helpText("Topic"),
  ## content
  page1_content_li1 = helpText("Do the lockdown measures related to the corona-virus pandemic impact groud level air pollution? "),
  page1_content_li2 = helpText("Our definition of Effect: all consequences of covid on environment, e.g. reduced traffic and production"),
  page1_content_li3 = helpText("Issues:"),
  page1_content_li3li1 = helpText("Endogeneity: Lockdown measures might be impacted by airpollution (reverse effect)"),
  page1_content_li3li2 = helpText("Controlling for missing Confounders that impact both"),
  page1_content_li3li3 = helpText("High Variance and Seasonalities in Airpollution"),
  
  # Approach
  ## title
  page2_title = helpText("Approach"),
  ## content
  page2_content1 = helpText("Difference in Difference Study Design: Stringency of Goverment measures as continuous confounder"),
  page2_content2 = helpText("Identifying Assumption: Common Trend of Treated and Non-Treated"),
  page2_content_li1 = helpText("Need to include all confounders that violate the common trend assumption (Group 0 and Group1 need to behave similarly)"),
  page2_content_li2 = helpText("Estimation Method: Fixed Effect / Pooled OLS over 30 days in Covid (20 March – 20 April)– 30 days right before Covid (February)"),
  page2_content_li3 = helpText("2 versions to reduce variance of prediction:"),
  page2_content_li3li1 = helpText("Use difference to value previous year as y – reduces yearly seasonal effects (which are strong in case of airpollution"),
  page2_content_li3li2 = helpText("Use deviation from predicted airpollution from weather model as y. -> in a next step if weather also impacts stringency (likely because it impacts spread of covid) it could also be added as true confounder to model"),
  page2_content_li4 = helpText("Additionally: use Continents / Regions as Populatins s.t. common trend assumption is more likely to hold"),
  page2_content_li5 = helpText("Maybe: mention there is no true non-treated group, but degree of stringency is relevant -> this is used for group differences"),
  
  # Data
  ## title
  page3_title = helpText("Data"),
  ## content
  page3_box1_content1 = helpText("Open AQ"),
  page3_box1_stat1 = helpText("595"),
  page3_box1_content2 = helpText("mio air quality observations"),
  page3_box1_content3 = helpText("OpenAQ harmonizes disparate air quality data from across the world. Furthermore it stores the only temporary available data and provides access to the full data set via AWS Athena."),
  page3_box2_content1 = helpText("Weather Data"),
  page3_box2_stat1 = helpText("16"),
  page3_box2_content2 = helpText("mio weather observations"),
  page3_box2_content3 = helpText("Dataset of the world metrological organisation containing per day weather data."),
  page3_box3_content1 = helpText("Mobility"),
  page3_box3_stat1 = helpText("400"),
  page3_box3_content2 = helpText("tsd data points on activity information"),
  page3_box3_content3 = helpText("The dataset shows how visits and length of stay at different places change compared to a baseline. These changes are calculated, using the same kind of aggregated and anonymized data used to show popular times for places in Google Maps."),
  
  # Results I
  ## title
  page4_title = helpText("Results I"),
  ## content
  page4_content1 = helpText("Some Text about Stringency Model"),
  page4_selector = selectInput("stringencyCountryChoice","Select a Country",c("pick one", unique(all_data_over_time$CountryName)), "pick one"),
  page4_plot = plotOutput("stringencyPlot"),
  
  # Results II
  ## title
  page5_title = helpText("Results II"),
  ## content
  page5_content1 = helpText("Some Text about Weather Model"),
  page5_selector = selectInput("particleTypeWeatherChoice", "Select a Particle", c("pick one", unique(germany_pollution$parameter)), "pick one"),
  page5_plot = plotOutput("weatherModelPlot")
  
  )