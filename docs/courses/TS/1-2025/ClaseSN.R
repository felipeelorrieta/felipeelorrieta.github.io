#Paquetes Necesarios
require(TSA)
require(forecast)
require(fUnitRoots)
require(lmtest)
require(astsa)

###Ejemplo Datos Climaticos (SARIMA)

setwd("/Users/felocarril/Dropbox/Series de Tiempo/Curso Soche")
data<-read.table("Aeropuerto.csv",header=T,sep=";")
Temp<-ts(data[,4],start=c(1976,1),frequency=12)
ts.plot(Temp,main="Temperatura Promedio Estacion Arturo Merino Benitez",cex.main=2,ylab="",xlab="Tiempo",ylim=c(min(Temp)*0.95,max(Temp)*1.05))

model=auto.arima(Temp)
tsdiag(model,gof.lag=36,lwd=2)

y=log(Temp)
ts.plot(y,main="Temperatura Promedio Estacion Arturo Merino Benitez",cex.main=2,ylab="",xlab="Meses",ylim=c(min(y)*0.95,max(y)*1.05))


par(mfrow=c(2,1))
acf(y,lag.max=50,lwd=2,main="") 
pacf(y,lag.max=50,lwd=2,main="") 

#Original Time Series
adfTest(y, lags = 1, type = c("nc"))
adfTest(y, lags = 12, type = c("nc"))
var(y)
#1-0 Differenced Time Series
adfTest(diff(y), lags = 1, type = c("nc"))
adfTest(diff(y), lags = 12, type = c("nc"))
var(diff(y))
#0-1 Differenced Time Series
adfTest(diff(y,12), lags = 1, type = c("nc"))
adfTest(diff(y,12), lags = 12, type = c("nc"))
var(diff(y,12))
#2-0 Differenced Time Series
adfTest(diff(diff(y)), lags = 1, type = c("nc"))
adfTest(diff(diff(y)), lags = 12, type = c("nc"))
var(diff(diff(y)))
#1-1 Differenced Time Series
y1=diff(y)
adfTest(diff(y1,12), lags = 1, type = c("nc"))
adfTest(diff(y1,12), lags = 12, type = c("nc"))
var(diff(diff(y),12))

#1-2 Differenced Time Series
y2=diff(y1,12)
adfTest(diff(y2,12), lags = 1, type = c("nc"))
adfTest(diff(y2,12), lags = 12, type = c("nc"))
var(diff(diff(diff(y),12),12))
#0-2 Differenced Time Series
y3=diff(y,12)
adfTest(diff(y3,12), lags = 1, type = c("nc"))
adfTest(diff(y3,12), lags = 12, type = c("nc"))
var(diff(diff(y,12),12))

par(mfrow=c(2,1))
y2=diff(y,12)
acf(y2,lag.max=50,lwd=2,main="") 
pacf(y2,lag.max=50,lwd=2,main="")
ggtsdisplay(y2,lag.max=60)

model4=arima(y,order=c(2,0,2),seasonal=list(order=c(0,1,1),period=12))
summary(model4)
coeftest(model4)
tsdiag(model4,gof.lag=36)
Box.test(residuals(model4),lag=24)


#SARIMA astsa
modelo4<-sarima(y, 1,0,1, 0,1,1,12,no.constant=TRUE)
modelo4$ttable


Forecast<-predict(model4,n.ahead=24)
expFore<-ts(c(exp(y[length(y)]),exp(Forecast$pred)),start=c(2023,4),frequency=12)
ts.plot(exp(y), expFore, col = c("black", "red"), 
        xlab = "", ylab = "", lwd = 2, lty = 1:2, gpars = list(bty = "n", tcl = 0),main="Forecast proximos 24 meses")
lines(exp(Forecast$pred+2*Forecast$se),col="blue",lty=3)
lines(exp(Forecast$pred-2*Forecast$se),col="blue",lty=3)

p=trunc(0.9*length(y))
y1=y[1:p]
model4val=arima(y1,order=c(2,0,2),seasonal=list(order=c(0,1,1),period=12))
Forecastval<-predict(model4val,n.ahead=57)
error=y[(p+1):length(y)]-Forecastval$pred
MSE=mean(error**2)
expForeval<-ts(exp(Forecastval$pred),start=c(2018,8),frequency=12)
expForese<-ts(exp(2*Forecastval$se),start=c(2018,8),frequency=12)
ts.plot(exp(y), expForeval, col = c("black", "red"), 
        xlab = "", ylab = "", lwd = 2, lty = 1:2, gpars = list(bty = "n", tcl = 0),main=paste("Validacion Poder Predictivo MSE",round(MSE,4)))
lines(expForeval*expForese,col="blue",lty=3)
lines(expForeval/expForese,col="blue",lty=3)

ts.plot(exp(y), expForeval, col = c("black", "red"), 
        xlab = "", ylab = "", lwd = 2, lty = 1:2, gpars = list(bty = "n", tcl = 0),main=paste("Validacion Poder Predictivo MSE",round(MSE,4)),xlim=c(2016,2023))
lines(expForeval*expForese,col="blue",lty=3)
lines(expForeval/expForese,col="blue",lty=3)