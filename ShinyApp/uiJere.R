###############################################################################
# Entrypoint for the shiny app
#
# Author: Fabian Karst
# Created 2020-05-20

###############################################################################
library(shiny)

ui = shiny::htmlTemplate(
  # Index Page
  "www/index2.html",
  
  # home page
  ## title
  page_title = textOutput(
    "Test title",
    inline = T
  ),
  
  ## subtitle
  page_subtitle = textOutput(
    "page_subtitle_text",
    inline = T
  )
)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Weather and Pollution"),

  sidebarPanel(
    selectInput("variable", "Variable:",
                list("pm10" = "pm10", 
                     "co" = "co", 
                     "no2" = "no2",
                     "o3" = "o3"))
    #,   checkboxInput("outliers", "Show outliers", FALSE)
  ),

  mainPanel(h3(textOutput("caption")),

    plotOutput("germanyPlot"))
))