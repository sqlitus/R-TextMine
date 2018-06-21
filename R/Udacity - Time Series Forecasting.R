#### Udacity - Time Series Forecasting ####

#### Lesson 1: Fundamentals of Time Series Forecasting ----

## sample problem: bates hotel booking monthly data
# explore data
# trend & seasonal components
# apply findings to ARIMA & ETS model
# forecast next 6 months

## Simple Forecasting Methods
# Average: avg of everything previous
# Moving AVerage: last x time intervals
# Naive: all forecasts equal to last observation
# Seasonal Naive: (forecast equal to same [time] last [period]?). Magnitude of seasonal pattern will remain consistent.
# Exponential Smoothing: ?

# Cyclical pattern: not a fixed period. usually longer & bigger than seasonal, & harder to predict
# Seasonal pattern: unchanging & associated with calendar



#### Lesson 2: ETS Models ----

## time series = error, trend, seasonality
# seasonality - seasonal patterns. intervals. constant or increasing or none.
# trend - "tendency" centered moving average. deseasonalized. linear or exponential or none.
# error - difference between observed value & trendline estimate. "remainder"

## additive vs multiplicative
# additive method: useful when trend & seasonal variation are constant. linear. e.g. +1000 bookings
# multiplicative: "..." increases or decreases. exponential. e.g. x1.5 bookings. when things change over time.


# Simple Exponential Smoothing Method
  # weight the most recent obs. alpha. 0 to 1. damping = 1 - alpha, applied to last forecast amount...
# Holt's Linear Trend Method. trend + level calculations, applied additively (linear trend line)
# Exponential Trend Method. level + trend components, applied multiplicatively (exponential trend line)
# Dampted trend measure. Phi. low = less change
# Holt-Winters Seasonal Method. 3 smoothing equation: level, trend, seasonal. often used with damped parameter.

library(tidyverse)

# decompose ETS
ets.data <- readxl::read_xlsx("C:\\Users\\chris.jabr\\Downloads\\champagne-sales.xlsx")
myts <- ts(ets.data$`Champagne Sales`, frequency = 12, start = c(2000,1))
myts
plot(myts)
stl(myts, s.window = "periodic") %>% plot()
decompose(myts)
decompose(myts) %>% plot()

# simple moving average
# http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
library(TTR)
SMA(myts, n = 3)
plot(myts)
plot.ts(SMA(myts, n = 3))
plot.ts(SMA(myts, n = 6))
plot.ts(SMA(myts))
plot.ts(SMA(myts, n = 15))

# plot seasonally adjusted
decompose(myts)
plot(myts)
plot(decompose(myts))
plot(decompose(myts)$trend)
plot(decompose(myts)$seasonal)
plot(decompose(myts)$random)

myts - decompose(myts)$seasonal
plot(myts - decompose(myts)$seasonal) # seasonality removed.
plot(myts - decompose(myts)$random)



# Lesson 3 : ARIMA ----
# AR: p = periods
# I:  d = differencing. number of times differencing needed to make the plot stationary
# MA: q = lags of error

# stationary time series: mean & variance constant over time (around 0)
# helps mean & variance
# use 'differencing' to turn a time series into stationary
ets.data <- ets.data %>% mutate(lag = lag(`Champagne Sales`)) %>%
  mutate(d1 = `Champagne Sales` - lag) %>%
  mutate(d2 = d1 - lag(d1))
plot.ts(ts(ets.data$d1, frequency = 12, start = c(2000,1)))









#### Time Series Forecasting in R | Edureka ####
library(tidyverse)
data("AirPassengers")
View(AirPassengers)
attributes(AirPassengers); start(AirPassengers); end(AirPassengers); summary(AirPassengers)
as_data_frame(AirPassengers) %>% View()
plot(AirPassengers)
abline(reg = lm(AirPassengers ~ time(AirPassengers))) # mean. changes by timeframe. average of two points? something more.
time(AirPassengers)
cycle(AirPassengers)










# reference: random numbers and sequences and time series and forecasting functions ----
library(tidyverse)
seq(1,5)
rnorm(5)
rep(5,1)
rep(5,11)
rep(rnorm(1),5)
rep(rnorm(2),5)
runif(5, 1, 8)
sample(state.name, 5)
test <- sample(1:500, 72)

ts(test)
ts(test, frequency = 6)
# start = start year & period. frequency = periods per year.
myts <- ts(test, start=c(2009, 1), end=c(2014, 12), frequency=12)
myts
plot(myts)
# ETS plot
stl(myts, s.window = "periodic") %>% plot
HoltWinters(myts, beta = F, gamma = F)
HoltWinters(myts, beta = F, gamma = F)$x
HoltWinters %>% str()


# reference: ggplot time series and overlaying multiple time series line plots ----
# https://plot.ly/ggplot2/time-series/
ggplot(ets.data, aes(x = Month, y = `Champagne Sales`)) + geom_bar(stat = "identity")

# reference:plotly ----
plotly::plot_ly(ets.data, x = ~Month, y = ~`Champagne Sales`, type = )


#### reference: converting TS to dataframe and vis versa ----
data("AirPassengers")

# creates matrix
ts_to_df <- tapply(AirPassengers, list(year = floor(time(AirPassengers)), month = month.abb[cycle(AirPassengers)]), c)
ts_to_df
as_data_frame(ts_to_df) %>% View()


month.abb
month.abb %>% str()
class(month.abb)
cycle(AirPassengers)
time(AirPassengers)
month.abb[cycle(AirPassengers)]
month.abb[cycle(AirPassengers)][1:15]
month.abb[cycle(AirPassengers)][1:15][5:10][1:2]

# convert ts to matrix to df & gather
tt <- ts(rnorm(12*5, 17, 8), start=c(1981,1), frequency = 12)
tt
dmn <- list(month.abb, unique(floor(time(tt)))) # list of row & column names
dmn_df <- as.data.frame(t(matrix(tt, 12, dimnames = dmn))) # create matrix & transpose
dmn_df
gather(dmn_df, key = "Month", value = "Value") # automatically makes key & value pairs w/ all columns


unique()
unique(floor(time(tt)))
dmn
matrix(tt, 12, dimnames = dmn)
t(matrix(tt, 12, dimnames = dmn))

# Air Passengers ts
dmn_air <- list(month.abb, unique(floor(time(AirPassengers)))) # needs time
dmn_air_df <- as_data_frame(t(matrix(AirPassengers, 12, dimnames = dmn_air)))
gather(dmn_air_df) %>% View()
View(AirPassengers)

# list ts to df conversion example
list(year = floor(time(tt)), month = month.abb[cycle(tt)]) # get each year for ts and string of months
tapply(tt, list(year = floor(time(tt)), month = month.abb[cycle(tt)]), c)