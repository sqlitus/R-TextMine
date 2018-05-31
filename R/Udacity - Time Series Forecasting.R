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
# multiplicative: "..." increases or decreases. exponential. e.g. x1.5 bookings.


# Simple Exponential Smoothing Method
# Holt's Linear Trend Method
# Exponential Trend Method
# Holt-Winters Seasonal Method






# reference: random numbers and sequences and time series and forecasting functions
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
ts(test, start=c(2009, 1), end=c(2014, 12), frequency=12)
ts(test, start=c(2009, 1), end=c(2014, 12), frequency=6)
ts(test, start=c(2009, 1), end=c(2014, 12), frequency=1)
myts <- ts(test, start=c(2009, 1), end=c(2014, 12), frequency=12)
plot(myts)
stl(myts, s.window = "periodic") %>% plot
HoltWinters(myts, beta = F, gamma = F)
HoltWinters(myts, beta = F, gamma = F)$x
HoltWinters %>% str()