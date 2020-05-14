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

# data over time
all_data_over_time <- fread(paste0(inpath,"all_data_over_time.csv"), encoding="utf-8")

all_data_over_time = arrange(setDF(all_data_over_time), CountryName, Date)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Reactivity"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      uiOutput("StringencyChoiceCountry")
      
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
      plotOutput("stringencyPlot")
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$StringencyChoiceCountry = renderUI(selectInput("stringencyCountryChoice","Select a Country",c(unique(all_data_over_time$CountryName),"pick one"), "pick one"))
  
  country_data_stringency = reactive(all_data_over_time[all_data_over_time$CountryName== input$stringencyCountryChoice,])
  
  output$stringencyPlot <- renderPlot({

    if(is.null(input$stringencyCountryChoice)){return()
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

}

shinyApp(ui, server)
