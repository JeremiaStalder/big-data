###############################################################################
# Defining Server Logic behind App
#
# Author Fabian Karst, Erik Senn, Jeremia Stalder
# Created 2019-01-30 20:32:44
###############################################################################


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Dynamic UI inputs based on data
  # country selection for stringency
  output$StringencyChoiceCountry = renderUI(selectInput("stringencyCountryChoice","Select a Country",c("pick one", unique(all_data_over_time$CountryName)), "pick one"))
  
  # Particle Selection for weather model
  output$particleTypeWeatherChoice = renderUI(selectInput("particleTypeWeatherChoice","Select a Particle Type",c("pick one", unique(germany_pollution$parameter)), "pick one"))
  
  output$carPlot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  
  
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
