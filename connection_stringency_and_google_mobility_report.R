# Investigate Relationship Oxford Covid Tracker & Google Mobility Data

# librarys
library(tidyverse)
library(readr)
library(zoo)

outpath = "./output/stringency_and_mobility/"

# functions ----
## plotting function 
line_plot_multiple = function(title, outpath,x,xlab, ylab, names_y, y_percent, legend, y1, y2,y3,y4,y5,y6,y7,y8,y9,y10, y11) {
  # plots up to 10 lines with same scale 
  # insert vectors for x, y1, y2... y2-10 are optional
  # give vector for legend entry names_y, ...
  # if names_y na or missing colors are still given
  # legend: boolean if legend should be created, Default is TRUE
  if(missing(xlab)) {xlab=NULL}
  if(missing(ylab)) {ylab=NULL}
  if(missing(title)) {title=NULL}
  if(missing(names_y)==T | all(is.na(names_y))==T) {names_y=c(1:11)}
  if(missing(legend)){legend = T}
  if(missing(y_percent)){y_percent = FALSE}
  
  plot = ggplot(data=NULL, aes(x=x))+
    geom_line(aes(y=y1, col = names_y[1])) 
  
  # only add layers if value provided
  if (missing(y2)==F) {
    plot = plot + geom_line(aes(y =y2 , col =names_y[2]))
  }  
  if (missing(y3)==F) {
    plot = plot + geom_line(aes(y =y3 , col =names_y[3]))
  }  
  if (missing(y4)==F) {
    plot = plot + geom_line(aes(y =y4 , col =names_y[4]))
  }  
  if (missing(y5)==F) {
    plot = plot + geom_line(aes(y =y5 , col =names_y[5]))
  }  
  if (missing(y6)==F) {
    plot = plot + geom_line(aes(y =y6 , col =names_y[6]))
  }  
  if (missing(y7)==F) {
    plot = plot + geom_line(aes(y =y7 , col =names_y[7]))
  }  
  if (missing(y8)==F) {
    plot = plot + geom_line(aes(y =y8 , col =names_y[8]))
  }  
  if (missing(y9)==F) {
    plot = plot + geom_line(aes(y =y9 , col =names_y[9]))
  }  
  if (missing(y10)==F) {
    plot = plot + geom_line(aes(y =y10 , col =names_y[10]))
  }  
  if (missing(y11)==F) {
    plot = plot + geom_line(aes(y =y11 , col =names_y[11]))
  }  
  
  plot = plot + theme_bw()+
    labs(x = xlab)+
    labs(y = ylab)
  
  # add legend if Legend is true are given
  if(legend==T) {
    plot = plot + theme(legend.title = element_blank(), legend.position = "bottom", legend.box.background = element_rect(colour = "black"))
  } else {
    plot = plot + theme(legend.position = "none")
  }
  
  # add units to yaxis (e.g. percent)
  if(y_percent==T) {
    plot = plot + scale_y_continuous(labels = scales::percent_format(accuracy = 2))
  }
  
  # other plot options
  plot = plot +  ggtitle(paste(title,sep=" ")) +
    theme(plot.title = element_text(size=10, face="bold"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10,face="bold"))+
    ggsave(file=paste0(outpath,title,".png"), width=6, height=4, dpi=600)
  plot
  return(plot)
}


### import data ----
nationwide_data_clean <- read_csv("data/clean/nationwide_data_clean.csv") # load nationwide data
global_mobility_report <- read_delim("data/google_mobility/Global_Mobility_Report.csv", 
                                     ";",col_types = cols(date = col_date(format = "%d/%m/%Y")), escape_double = FALSE, trim_ws = TRUE) # load Google Mobility Data

### cleaning global_mobility_report ----

  # summary 
  summary(is.na(global_mobility_report))
  summary(global_mobility_report)

  # rename cols to match nationwide data
  colnames(global_mobility_report)[1] = "CountryCode" 
  colnames(global_mobility_report)[2] = "CountryName" 
  colnames(global_mobility_report)[5] = "Date" 

  # change CountryCode from na to "na" for namibia. Remove countries without code
  global_mobility_report = mutate(global_mobility_report, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode))
  global_mobility_report = global_mobility_report[is.na(global_mobility_report$CountryCode)==F,] # only take countries with country code
  
  # drop observations with NA values for indices
    # drop if X or more indices are NA
    max_number_missing_indices = 6 # 0==all indices necessary, 6== no index necessary
    global_mobility_report = filter(global_mobility_report, (is.na(retail_and_recreation_percent_change_from_baseline) +
                                                              is.na(grocery_and_pharmacy_percent_change_from_baseline) +
                                                              is.na(parks_percent_change_from_baseline) +
                                                              is.na(transit_stations_percent_change_from_baseline) +
                                                              is.na(workplaces_percent_change_from_baseline) +
                                                              is.na(residential_percent_change_from_baseline))<=max_number_missing_indices)
  # shorten variable names
    mobility_variables_original = c("retail_and_recreation_percent_change_from_baseline",
                           "grocery_and_pharmacy_percent_change_from_baseline",
                           "parks_percent_change_from_baseline",
                           "transit_stations_percent_change_from_baseline",
                           "workplaces_percent_change_from_baseline",
                           "residential_percent_change_from_baseline")
    mobility_variables = sapply(strsplit(mobility_variables_original,split="_percent_change_from_baseline"), function(x) (x[1]))
    colnames(global_mobility_report)[colnames(global_mobility_report) %in% mobility_variables_original] = mobility_variables
  

    
# clean short for nationwide data
  nationwide_data_clean = mutate(nationwide_data_clean, CountryCode = ifelse(is.na(CountryCode) & CountryName=="Namibia", "NA", CountryCode)) #redo because "NA" is imported as NA
  
  
                                                                      
###  merge data (country level) ----
  
   # inner join by CountryCode and Date. Remove obs where one of the datapoints is not given. Remove CountryName of nationwide data
   merged_data = inner_join(global_mobility_report, select(nationwide_data_clean,-CountryName), by = c("CountryCode", "Date")) 
  
  # get only country data
    merged_data_countries = filter(merged_data, is.na(sub_region_1))
    
    
### Descriptives ----

    # summary 
    summary(is.na(merged_data_countries))
    summary(merged_data_countries)
    
    
   # Correlations
    # Worldwide
    print("Strong Negative Correlation of Stringency and Mobility Variables - overall Cross Section")
    print("Grocery/Pharmacy and Parks lower correlation than others")
    print("Residential strong positive correlation (obviously)")
    cor(merged_data_countries[,c("StringencyIndex", mobility_variables)] ,use = "pairwise.complete.obs")
      
    # Plot 
      # Worldwide simple averages
      stringency_mobility_ww_av = group_by(merged_data_countries, Date) %>%
        summarize(StringencyIndex = mean(StringencyIndex,na.rm=T), 
                  retail_and_recreation=  mean(retail_and_recreation, na.rm=T), 
                  grocery_and_pharmacy=  mean(grocery_and_pharmacy, na.rm=T), 
                  parks=  mean(parks, na.rm=T), 
                  transit_stations=  mean(transit_stations, na.rm=T), 
                  workplaces=  mean(workplaces, na.rm=T), 
                  residential=  mean(residential, na.rm=T), 
                  ConfirmedCases=sum(ConfirmedCases,na.rm=T))
      
      # get plot variables
      y1 = stringency_mobility_ww_av$StringencyIndex
      y2 = log(stringency_mobility_ww_av$ConfirmedCases)*5 # log case number time random scalar for better depiction
      
      for (i in 1:(length(mobility_variables))) { # mobility vars + 1(stringency) +1(infected)
        name = paste0("y",i+2)
        data = select(stringency_mobility_ww_av, mobility_variables[i])
        colnames(data) = "var"
        assign(name, -data$var)
      }
      
      line_plot_multiple("Stringency and Mobility - Worldwide", outpath, stringency_mobility_ww_av$Date,"Date", "Stringency and Mobility (Mobility Change *-1)", names_y=c("Stringency Index", "log(ConfirmedCases)*5(to see better)", mobility_variables),
                         y_percent=F, legend=T, y1, y2,y3,y4,y5,y6,y7,y8)
      
### Models ----
    
  
  
  
  
  
  