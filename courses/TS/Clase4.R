#############################
###### Modelo AR ###########
#############################

##Reconociendo un AR(1)
set.seed(1)
x<-arima.sim(n = 1000, list(ar = c(0.5)))
plot(x)
t<-seq(1:1000)
summary(lm(x~t))
par(mfrow=c(2,1))
acf(x,lag.max=20,lwd=2)
pacf(x,lag.max=20,lwd=2)
acf(x)$acf
modelo<-ar.yw(x,order=1,demean=F)
modelo$asy.var.coef
(1-modelo$ar^2)/1000
t=modelo$ar/sqrt(modelo$asy.var.coef)
modelo<-ar(x,method="yule-walker")
ajuste <- arima(x, c(1, 0, 0)) #c(1,0,0) es un AR(1)
tsdiag(ajuste,gof.lag=20)

##Reconociendo un AR(1)
set.seed(1)
x<-arima.sim(n = 1000, list(ar = c(-0.5)))
plot(x)
t<-seq(1:1000)
summary(lm(x~t))
par(mfrow=c(2,1))
acf(x,lag.max=100)
pacf(x,lag.max=100)
acf(x)$acf
ajuste <- arima(x, c(1, 0, 0))
tsdiag(ajuste)


##Reconociendo un AR(2)
set.seed(1)
x<-arima.sim(n = 1000, list(ar = c(-0.5,-0.2)),sd = sqrt(0.1796))
plot(x)
t<-seq(1:1000)
summary(lm(x~t))
par(mfrow=c(2,1))
acf(x,lag.max=20,lwd=2)
pacf(x,lag.max=20,lwd=2)
acf(x)$acf
modelo<-ar.yw(x,order=2)
modelo$asy.var.coef
t1=modelo$ar[1]/sqrt(modelo$asy.var.coef[1,1])
t2=modelo$ar[2]/sqrt(modelo$asy.var.coef[2,2])
ajuste <- arima(x, c(0, 0, 1))
tsdiag(ajuste)

ajuste <- arima(x, c(0, 0, 1))
tsdiag(ajuste)

#############################
###### Modelo MA ###########
#############################

##Reconociendo un MA(1)
set.seed(1)
x<-arima.sim(n = 1000, list(ma = c(-0.5)))
plot(x)
t<-seq(1:1000)
summary(lm(x~t))
par(mfrow=c(2,1))
acf(x,lag.max=50,lwd=2)
pacf(x,lag.max=50,lwd=2)
acf(x)$acf
ajuste <- arima(x, c(0, 0, 1))
tsdiag(ajuste)
ajuste <- arima(x, c(4, 0, 0))
tsdiag(ajuste)

set.seed(1)
x<-arima.sim(n = 1000, list(ma = c(0.5)),sd = sqrt(0.1796))
plot(x)
t<-seq(1:1000)
summary(lm(x~t))
par(mfrow=c(1,2))
acf(x,lag.max=100)
pacf(x,lag.max=100)
acf(x)$acf
ajuste <- arima(x, c(0, 0, 1))
tsdiag(ajuste)


##Reconociendo un MA(2)
set.seed(1)
x<-arima.sim(n = 1000, list(ma = c(-0.5,0.7)),sd = sqrt(0.5))
plot(x)
t<-seq(1:1000)
summary(lm(x~t))
par(mfrow=c(2,1))
acf(x,lag.max=20,lwd=2)
pacf(x,lag.max=20,lwd=2)
acf(x)$acf
ajuste <- arima(x, c(0, 0, 1))
tsdiag(ajuste)
ajuste2 <- arima(x, c(2, 0, 0))
tsdiag(ajuste2)
ajuste$aic
ajuste2$aic

x<-arima.sim(n = 1000, list(ar = c(0.3,0.5,-0.7)),sd = sqrt(0.5))
acf(x)
pacf(x)