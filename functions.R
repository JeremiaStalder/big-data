# Functions ----
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

remove_outliers <- function(vect) {
  quantile1 <- quantile(vect, probs=c(.25, .75), na.rm = TRUE)
  quantile2 <- quantile(vect, probs=c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(vect, na.rm = TRUE)
  vect[vect < (quantile1[1] - H)] <- quantile2[1]
  vect[vect > (quantile1[2] + H)] <- quantile2[2]
  return(vect)
}


ppm_to_microgram <- function(particle,tempF, pressure_millibars_to_tenth, concentration_pmm, constant_table){
  # convert ppm to microgramm/m3 according to https://www.ccohs.ca/oshanswers/chemicals/convert.html
  # inputs: temperature in Fahrenheit, pressure in millibars to tenth, concentration in ppm
  # output: concentraction in microgram/m3
  # Note: make sure inputs are cleaned. NAs are allowed
  
  # replace non-positive values with defaults for temperature, pressure, replace with NA for concentration_pmm
  tempF = replace_na(tempF,filter(constant_table, key== "subtract_f")$value + filter(constant_table, key== "temperature")$value / filter(constant_table, key== "factor_f")$value)
  tempC = (tempF-filter(constant_table, key== "subtract_f")$value)*filter(constant_table, key== "factor_f")$value
  tempC = ifelse(tempC>-filter(constant_table, key== "kelvin_to_celsius")$value , tempC, NA)
  
  
  pressure_mmHg = ifelse(pressure_millibars_to_tenth>0 , pressure_millibars_to_tenth * filter(constant_table, key== "convert_pressure")$value, NA)
  pressure_mmHg = replace_na(pressure_mmHg,filter(constant_table, key== "pressure")$value)
  
  concentration_pmm = ifelse(concentration_pmm>0 , concentration_pmm, NA)
  
  # calculate concentration
  volume_1_gram = filter(constant_table, key== "G")$value * (tempC+ filter(constant_table, key== "kelvin_to_celsius")$value)  / pressure_mmHg # calc volume
  mol_mass =  constant_table$value[match(particle, filter(constant_table, unit== "g/mol")$key)] # calc mol mass
  concentration_microgram_per_m3 = 10^3 * mol_mass * concentration_pmm / volume_1_gram # calc concentration 
  
  return(concentration_microgram_per_m3)
}









