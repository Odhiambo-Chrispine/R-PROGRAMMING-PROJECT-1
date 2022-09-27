library(quantmod)
library(tseries)
library(forecast)
library(xts)
install.packages("timeSeries")

DATA=read.csv("Uk_largest_companies.csv")
View(DATA)
attach(DATA)

getSymbols(from="12/04/2010" ,to="20/06/2014")
class(Profits...billion..)
plot(Profits...billion..)

par(mfrow=c(1,2))
Acf(Profits...billion..)
Pacf(Profits...billion..)

print(adf.test(Profits...billion..)) #p-value=0.1125
auto.arima(Profits...billion..,seasonal=FALSE)

fitA=auto.arima(Profits...billion..,seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max = 40)
auto.arima(Profits...billion..,seasonal = FALSE)

fitB=arima(Profits...billion..,order = c(1,1,4))
tsdisplay(residuals(fitB),lag.max = 40)

fitC=arima(Profits...billion..,order = c(5,1,4))
tsdisplay(residuals(fitC),lag.max = 40)

fitD=arima(Profits...billion..,order = c(1,1,1))
tsdisplay(residuals(fitD),lag.max = 40)

#plots of arima models
par(mfrow=c(2,2))
#auto arima (2,0,2)
term=100 #number of days
fcast1=forecast(fitA,h=term)
plot(fcast1)
#custom arima (3,0,3)
fcast2=forecast(fitB,h=term)
plot(fcast2)
fcast3=forecast(fitC,h=term)
plot(fcast3)
fcast4=forecast(fitD,h=term)
plot(fcast4)

#mape accuracy subtract from 100
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)

#forcast
forecast(fitA,h=term)
forecast(fitB,h=term)
forecast(fitC,h=term)
forecast(fitD,h=term)



#ARIMA FORECASTING PROFITS(timeseries)
profit=ts(Profits...billion..,frequency = 4)

library(forecast)
library(tseries)

par(mfrow=c(1,1)) #determining the size of the plot
plot(profit)

adf.test(profit) #since p-value is less than 0.5, then the data is stationary

profitmodel=auto.arima(profit,ic="aic",trace = TRUE) #converting into arima model
profitmodel #checking the best model for profit

par(mfrow=c(2,2))
acf(ts(profitmodel$residuals))
pacf(ts(profitmodel$residuals))

profitforecast=forecast(profitmodel,level = c(95),h=10*4) #h->next 10 years, level->confidence level
profitforecast

par(mfrow=c(1,1))
plot(profitforecast)

Box.test(profitforecast$residuals,lag=15,type = "Ljung-Box") #it is stationary since p-value<0.5


