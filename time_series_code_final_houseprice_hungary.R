rm(list=ls())

#Get Directory
getwd()

#Setting the directory to load the data
setwd("E:/Stats/Time Series")

#loading the data into R
#To load the data from excel we have to use readxl library

library(readxl)

hungary<-read_excel("time_series.xlsx")

#To view the loaded data
View(hungary)

#Converting the loaded data into time series
#Quarterly time series
hungary_ts<-ts(hungary[,2], start=2017, end=c(2019,4), frequency = 4)
hungary_ts
str(hungary_ts)
class(hungary_ts)
frequency(hungary_ts)

#Plotting the time series
plot.ts(hungary_ts)

#Transformation of time series in case of seasonal and random fluctuations
library('TTR')
hungary_ts_log<-log(hungary_ts)
plot.ts(hungary_ts_log)

#smoothing the time series
#Simple Moving Average
hungary_sma2<-SMA(hungary_ts, 2)
plot.ts(hungary_sma2)


#Exponential smoothing
hungary_ema2_25<-EMA(hungary_ts,1,ratio = .75)
plot.ts(hungary_ema2_25)
hungary_ema2_25


#Decompose
plot.ts(hungary_ts)
hungary_ts_decomp<-decompose(hungary_ts,type=c("multiplicative"))
hungary_ts_decomp
plot(hungary_ts_decomp)
hungary_ts_decomp

#Holtwinters method to forecast
hungary_ts_hw<-HoltWinters(hungary_ts)
hungary_ts_hw

hungary_ts_hw$fitted

library(forecast)

hungary_ts_hw_fcst<-forecast:::forecast.HoltWinters(hungary_ts_hw, h=4)
forecast:::plot.forecast(hungary_ts_hw_fcst)

#Forecasting the time series that has only seasonality component
#Beta=Trend and Gamma=Seasonality
hungary_quarterly_forecast<-HoltWinters(hungary_ts, gamma=FALSE)
hungary_quarterly_forecast
plot(hungary_quarterly_forecast)

#Forecasting the future values
hungary_quarterly_forecast_future <-forecast:::forecast.HoltWinters(hungary_quarterly_forecast,h=10)
forecast:::plot.forecast(hungary_quarterly_forecast_future)

#Testing the above method good or not
acf(na.exclude(hungary_quarterly_forecast_future$residuals), lax.max=10)
pacf(na.exclude(hungary_quarterly_forecast_future$residuals), lax.max=10)
Box.test(hungary_quarterly_forecast_future$residuals, lag=3, type='Ljung-Box')
plot.ts(hungary_quarterly_forecast_future$residuals)
hist(hungary_quarterly_forecast_future$residuals)
accuracy(hungary_quarterly_forecast_future) # RMSE 3.481844 
#Also in histogram it can be clearly seen that graph is left skewed#

#ARIMA Method to forecast
library(forecast)
plot.ts(hungary_ts)

hungary_diff1<-diff(hungary_ts, differences = 1)
plot.ts(hungary_diff1)

hungary_diff2<-diff(hungary_ts, differences = 2)
autoplot(hungary_diff2)

acf(hungary_diff1, lag.max=8)
pacf(hungary_diff1, lag.max=10)
#pacf(hungary_diff1, lag.max=20, plot=F)
hungary_model<-arima(hungary_ts, order=c(0,1,0))
hungary_model
summary(hungary_model) # RMSE 4.792709
hungary_arima<-auto.arima(hungary_ts, d=1, D=1, trace=TRUE)
hungary_arima # RMSE 2.938224 AIC=40.72
summary(hungary_arima)
checkresiduals(hungary_arima)

#Testing the above produced model
Box.test(hungary_arima$residuals, lag=3, type='Ljung-Box')
qqnorm(hungary_arima$residuals)
qqline(hungary_arima$residuals)
#After running the box test we can see that the p value is greater than 0.5 so this is the best model to forecast#
#Forecast values

hungary_arima_forecast<-forecast(hungary_arima, h=8)
hungary_arima_forecast
plot(hungary_arima_forecast)

#AIC: The less is the AIC the better will be the model

#Accuracy
accuracy(hungary_arima)

#ME: Mean Error - Average of all errors in a set

#RMSE: Root Mean Squared Error - Squared difference between observed and predicted

#MAE: Mean Absolute Error - Absolute error between observed and predicted

#MPE: Mean percentage Error - Average of percentage error between actual and predicted

#MAPE: Mean absolute percentage Error - Measure of prediction accuracy

#MASE: Mean absolute squared error - Measures accuracy of forecast

#ACF1: Autocorrelation of errors at lag 1 - Er

#snaive method
fit<-snaive(hungary_ts) 
summary(fit) #RMSE 18.24342
checkresiduals(fit)
fit_forecast<-forecast(fit, h=8)
autoplot(fit_forecast)
accuracy(fit) #RMSE 18.24342
#sd value is more than the ARIMA method so rejecting

fit_ets<-ets(hungary_ts)
summary(fit_ets) # AIC = 58.91304
checkresiduals(fit_ets)  # RMSE 2.469842
fit_ets_forecast<-forecast(fit_ets, h=8)
autoplot(fit_ets_forecast)
accuracy(fit_ets) 
#AIC value is more than the ARIMA method so rejecting
