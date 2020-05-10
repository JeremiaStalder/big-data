# Seasonality Approaches for modelling airpollution----
# test decompose

# produce test time series object
drift = 1
sigma = 5
number_obs = 50

test_data = ts(sigma * sin(c(1:number_obs)*pi*0.5+0.5*pi) + drift *seq(number_obs), start =1, end = number_obs, frequency = number_obs)
test_data_diff = test_data - stats::lag(test_data)

pacf(test_data, lag.max = 60)

names_y = c("true","trend", "seasonal", "error")

decomposed_data <- decompose(test_data)

y1_ts = as.vector(decomposed_data$x)
y2_ts = as.vector(decomposed_data$trend)
y3_ts = as.vector(decomposed_data$seasonal)
y4_ts =as.vector(decomposed_data$random)

line_plot_multiple(paste("Decomposed TS - ", region), outpath,seq(1:length(y1_ts)),"Date", "Airpolltion", names_y=names_y, 
                   y_percent=F, legend=T,y1_ts, y2_ts,y3_ts,y4_ts)


# Decompose on real data

test_data = ts(y1$value, start =1, end = length(y1$value), frequency = length(y1$value))

pacf(test_data, lag.max = 50)
acf(test_data, lag.max = 50) # 

names_y = c("true","trend", "seasonal", "error")

decomposed_data <- decompose(test_data,type = ("multiplicative"))

y1_ts = as.vector(decomposed_data$x)
y2_ts = as.vector(decomposed_data$trend)
y3_ts = as.vector(decomposed_data$seasonal)
y4_ts =as.vector(decomposed_data$random)

line_plot_multiple(paste("Decomposed TS - ", region), outpath,seq(1:length(y1_ts)),"Date", "Airpolltion", names_y=names_y, 
                   y_percent=F, legend=T,y1_ts, y2_ts,y3_ts,y4_ts)

# Try FILTER
test_data = ts(y1$value, start =1, end = length(y1$value), frequency = 12)

arima = auto.arima(y = test_data, seasonal=F, ic="bic")

arima_2 = arima(test_data, c(0,1,0))

test_data_arima =  forecast(arima, h=10)
forecast(arima_2, h=10)

pacf(test_data, lag.max = 50)
pacf(test_data_arima$residuals, lag.max = 50)

names_y = c("true","trend", "seasonal", "error")

y1_ts = as.vector(decomposed_data$x)
y2_ts = as.vector(decomposed_data$trend)
y3_ts = as.vector(decomposed_data$seasonal)
y4_ts =as.vector(decomposed_data$random)

line_plot_multiple(paste("Decomposed TS - ", region), outpath,seq(1:length(y1_ts)),"Date", "Airpolltion", names_y=names_y, 
                   y_percent=F, legend=T,y1_ts, y2_ts,y3_ts,y4_ts)