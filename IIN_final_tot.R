
setwd("C:/Users/lenovo/Desktop/New folder (3)/IIN")
### Packages that need to be installed for Time series forecasting
library(forecast)
install.packages("fpp")
library(fpp)

#### Import the data to R environment
IIN2<-read.csv("IIN.csv" )
View(IIN2)
### Analyse the basic statistics of imported data
summary(IIN2)
# Plot time series data
tsdisplay(IIN2$PINTI17Q1)
## Applying box cox transformation
lambda = BoxCox.lambda(IIN2$PINTI17Q1)
tsdata2 = BoxCox(IIN2$PINTI17Q1, lambda=lambda)
tsdisplay(tsdata2)

nrow(IIN2)

IIN<-IIN2[100:272,]
#### Checking seasonal and Month plots
seasonplot(IIN$PINTI17Q1)
monthplot(IIN$PINTI17Q1)

library("TTR")

###install.packages("forecast")
library(forecast)
trend_IIN= ma(IIN2[,2], order = 4, centre = T)
plot(as.ts(trend_IIN))
lines(trend_IIN)
plot(as.ts(trend_IIN))

write.csv(trend_IIN, "trend_IIN.csv")

trend_IIN2<-as.data.frame(trend_IIN)
trend_IIN3<-trend_IIN2[3:278,]
IIN_f<-IIN2[3:278,]

View(trend_IIN2)
View(IIN_f)


detrend_IINA = IIN_f[,2] - trend_IIN3

View(detrend_IINA)


plot(as.ts(detrend_IINA))

library(tseries)
adf = adf.test(detrend_IINA) 
kpss = kpss.test(detrend_IINA) 
adf
kpss

acf(detrend_IINA, lag.max = 20)
pacf(detrend_IINA, lag.max = 20)


#Automatic Selection Algorithm - Fast
auto.arima(detrend_IINA, trace= TRUE, ic ="aic")

finalmodel = arima(detrend_IINA, order = c(2, 0, 1))
### Results of the final model
summary(finalmodel)



#### Residual analysis
res<-residuals(finalmodel)
res_final<-as.numeric(res)


library(car)
#### Autocorrelation check
durbinWatsonTest(res_final)

### final prediction
predict(finalmodel, n.ahead = 9)


############Testing

trend_IIN4<-trend_IIN2[3:270,]
IIN_ft<-IIN2[3:270,]
detrend_IINA = IIN_ft[,2] - trend_IIN4

finalmodel1 = arima(detrend_IINA, order = c(2, 0, 1) ,include.mean = FALSE )
### Results of the final model
summary(finalmodel1)

### final prediction
predict(finalmodel1, n.ahead = 9)

