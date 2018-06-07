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

# decompose ETS
ets.data <- readxl::read_xlsx("C:\\Users\\chris.jabr\\Downloads\\champagne-sales.xlsx")
myts <- ts(ets.data$`Champagne Sales`, frequency = 12, start = c(2000,1))
myts
plot(myts)
stl(myts, s.window = "periodic") %>% plot()
decompose(myts)
decompose(myts) %>% plot()
# simple moving average
library(TTR)
SMA(myts, n = 3)
plot(myts)
plot.ts(SMA(myts, n = 3))
plot.ts(SMA(myts))




# reference: random numbers and sequences and time series and forecasting functions
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


# reference: ggplot time series and overlaying multiple time series line plots
# https://plot.ly/ggplot2/time-series/
ggplot(ets.data, aes(x = Month, y = `Champagne Sales`)) + geom_bar(stat = "identity")