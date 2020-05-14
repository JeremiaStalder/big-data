###############################################################################
# Defining Server Logic behind App to explore UBER data
#
# Author: Vivek Katial
# Created 2019-01-30 20:32:44
###############################################################################

library(shiny)
library(datasets)
library(data.table)
library(dplyr)
library(tidyverse)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
germany_pollution <- fread("../data/predictionAirpollutionFromWeatherData/germany_rolling.csv")
co = filter(germany_pollution, parameter == "co")
pm10 = filter(germany_pollution, parameter == "pm10")
o3 = filter(germany_pollution, parameter == "o3")
no2 = filter(germany_pollution, parameter == "no2")
# mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  formulaText <- reactive({
    paste("Particle: ", input$variable)
  })

  output$caption <- renderText({
    formulaText()
  })

  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$germanyPlot <- renderPlot({
    
    if (input$variable == "co") {
    ggplot(co, aes(date)) + 
    geom_line(aes(y = value, colour = "co in µg/m³", group = 1)) + ## "o3 in µg/m³"
    geom_line(aes(y = temp, colour = "average temperature in fahrenheit", group = 2))+
    theme_bw()+
    ggtitle("30 day rolling average of temperature and co particles - Germany") +
    ylab(label= "temperature and o3 particles") +
    theme(legend.title = element_blank()) +
                theme(legend.position = "bottom") + 
                theme(plot.title = element_text(size=10, face="bold"))+
                theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10,face="bold"))}
    else if (input$variable == "o3") {
    ggplot(o3, aes(date)) + 
    geom_line(aes(y = value, colour = "o3 in µg/m³", group = 1)) + ## "o3 in µg/m³"
    geom_line(aes(y = temp, colour = "average temperature in fahrenheit", group = 2))+
    theme_bw()+
    ggtitle("30 day rolling average of temperature and o3 particles - Germany") +
    ylab(label= "temperature and o3 particles") +
    theme(legend.title = element_blank()) +
                theme(legend.position = "bottom") + 
                theme(plot.title = element_text(size=10, face="bold"))+
                theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10,face="bold"))}
    else if (input$variable == "no2") {
    ggplot(no2, aes(date)) + 
    geom_line(aes(y = value, colour = "no2 in µg/m³", group = 1)) + ## "o3 in µg/m³"
    geom_line(aes(y = temp, colour = "average temperature in fahrenheit", group = 2))+
    theme_bw()+
    ggtitle("30 day rolling average of temperature and no2 particles - Germany") +
    ylab(label= "temperature and o3 particles") +
    theme(legend.title = element_blank()) +
                theme(legend.position = "bottom") + 
                theme(plot.title = element_text(size=10, face="bold"))+
                theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10,face="bold"))}
    else if (input$variable == "pm10") {
    ggplot(pm10, aes(date)) + 
    geom_line(aes(y = value, colour = "pm10 in µg/m³", group = 1)) + ## "o3 in µg/m³"
    geom_line(aes(y = temp, colour = "average temperature in fahrenheit", group = 2))+
    theme_bw()+
    ggtitle("30 day rolling average of temperature and pm10 particles - Germany") +
    ylab(label= "temperature and o3 particles") +
    theme(legend.title = element_blank()) +
                theme(legend.position = "bottom") + 
                theme(plot.title = element_text(size=10, face="bold"))+
                theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10,face="bold"))}
  })
})
