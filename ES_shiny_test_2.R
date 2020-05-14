library(readr)
library(zoo)
library(stats)
library(forecast)
library(lubridate)
library(data.table)
library(tidyverse)


# laod data before app
  setwd("~/GitHub/big-data") # setwd
  inpath = "presentation/tables_map/"

# Stringency, Pollution, Prediction data over time
  all_data_over_time <- read_csv(paste0(inpath,"all_data_over_time.csv"),
                                 col_types = cols(Date = col_date(format = "%Y-%m-%d")))
  all_data_over_time = arrange(all_data_over_time, CountryName, Date)

# # Weather Model Predictions
  germany_pollution <- read_csv("data/predictionAirpollutionFromWeatherData/germany_rolling.csv",
                                col_types = cols(date = col_date(format = "%Y-%m-%d")))
  
 #  germany_pollution <- fread("../data/predictionAirpollutionFromWeatherData/germany_rolling.csv")
  
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Reactivity"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      uiOutput("StringencyChoiceCountry"),
      uiOutput("ParticleTypeWeatherChoice")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      
      # Plot Output
      plotOutput("stringencyPlot"),
      plotOutput("weatherModelPlot")
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Dynamic UI inputs based on data
    # country selection for stringency
    output$StringencyChoiceCountry = renderUI(selectInput("stringencyCountryChoice","Select a Country",c("pick one", unique(all_data_over_time$CountryName)), "pick one"))
  
    # Particle Selection for weather model
    output$ParticleTypeWeatherChoice = renderUI(selectInput("particleTypeWeatherChoice","Select a Particle Type",c("pick one", unique(germany_pollution$parameter)), "pick one"))
    
  # render plots
    # country selection for stringency
    output$stringencyPlot <- renderPlot({

      if(is.null(input$stringencyCountryChoice)){return()
      } else if (input$stringencyCountryChoice=="pick one"){return()
        } else {
        # Test via plot
        stringency_model_plot = group_by(all_data_over_time, sub_region_1) %>%
          filter(CountryName==input$stringencyCountryChoice) %>%
          arrange(Date)

        # change encoding for plot
        Encoding(stringency_model_plot$sub_region_1) = "latin1"

        title=paste("Predicted Stringency Model - States in", toupper(input$stringencyCountryChoice))
        ggplot(stringency_model_plot, aes(x = Date))+
          geom_line(aes(y=StringencyIndexCountry))+
          geom_line(aes(y=StringencyIndex, color = sub_region_1))+
          theme_bw()+
          ggtitle(paste(title,sep=" ")) +
          ylab(label= "Pred. Stringency 3 Day Av.") +
          theme(legend.title = element_blank()) +
          theme(legend.position = "bottom") +
          theme(plot.title = element_text(size=10, face="bold"))+
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=10,face="bold"))
        }
    })

    # Particle Selection for weather model
    output$weatherModelPlot <- renderPlot({

      if(is.null(input$particleTypeWeatherChoice)){return()
      } else if (input$particleTypeWeatherChoice=="pick one"){return()
      } else {
      germany_pollution_plot = filter(germany_pollution,parameter == input$particleTypeWeatherChoice)

      title= paste("30 Day Rolling Average of Temperature and", input$particleTypeWeatherChoice,"Particles - GERMANY")
      unit = paste(input$particleTypeWeatherChoice, "in µg/m³")
      y_label  = paste("temperature and", input$particleTypeWeatherChoice, "particles")
      
      ggplot(germany_pollution_plot, aes(x=date)) +
        geom_line(aes(y = value, colour = unit, group = 1)) + ## "o3 in µg/m³"
        geom_line(aes(y = temp, colour = "average temperature in fahrenheit", group = 2))+
        theme_bw()+
        ggtitle(title) +
        ylab(label= y_label) +
        xlab(label= "Date") +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=10, face="bold"))+
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=10,face="bold"))

        }
    })
}
    
shinyApp(ui, server)
