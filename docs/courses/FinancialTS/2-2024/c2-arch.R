library(TSA)
set.seed(912)
x=garch.sim(alpha=c(.3,.7),n=2000)
ts.plot(x,main='Simulacion Proceso ARCH(1)')

par(mfrow=c(2,1))
acf(x, lwd=3, main ="")
title(expression(paste("ACF ", y[t])))
acf(x^2,main="", lwd=3)
title(expression(paste("ACF ", y[t]^2)))
pacf(x^2,main="", lwd=3)
title(expression(paste("ACF ", y[t]^2)))

Box.Test = function(x, lag = 1, main = "p values for Ljung-Box statistic"){

B<-vector("numeric")
for(i in 1:lag){
B[i]<-Box.test(x, lag = i, type = "Ljung-Box")$p.value
}
A<-matrix(cbind(c(1:lag),B), nrow = lag, ncol = 2, dimnames = list(NULL, c("lag", "p.value")))
plot(A[,1], A[,2], ylim = c(0, max(0.051,(max(A[,2])+.01))), 
ylab = "p-value", xlab = "Lag", main = main)
abline(0.05, 0, col = 4, lty = 2)
return(A)
}

par(mfrow=c(2,1))
Box.Test(x,20, main= expression(paste(" valores-p Box Ljung ", y[t])))
Box.Test(x^2,20, main= expression(paste(" valores-p Box Ljung ", y[t]^2)))


###########################
## Ajuste modelo ARCH(1) ##
###########################

McLeod.Li.test(y=x)
require(tseries)
fit3   <- garch(x,order = c(0, 1) )  # Fit a ARCH(1)
summary(fit3)
plot(fit3)
res_1 <- na.omit(residuals(fit3)) 
Box.test(res_1,lag=10)
Box.test(res_1,lag=15)
Box.test(res_1,lag=18)
Box.test(res_1,lag=20)
pred10 = predict(fit3) 

require(rugarch)
spec1 <- ugarchspec(variance.model = list(garchOrder=c(1,0)), 
                   mean.model = list(armaOrder=c(0,0), include.mean=FALSE))
fit1 <- ugarchfit(spec1, x)

show(fit1)
v1=sigma(fit1)
ts.plot(as.numeric(v1))
summary(v1-pred10[,1])
coef(fit1)
infocriteria(fit1) #AIC
head(as.numeric(sigma(fit1)))**2 #Varianza Estimada
head(pred10[,1])**2
ts.plot(as.numeric(sigma(fit1)),ylab="",main='Volatilidad Estimada')

head(as.numeric(fitted(fit1)))  #Media Estimada
head(as.numeric(residuals(fit1))/as.numeric(sigma(fit1))) #Residuos
head(residuals(fit3))

plot(fit1)

forc = ugarchforecast(fit1, n.ahead=12)
plot(forc)



library(fGarch)

fit1 = garchFit(formula = ~ garch(1, 0), data = x, include.mean = FALSE, trace=F)
fit1

summary(fit1)
plot(fit1)

fit2 = garchFit(formula = ~ garch(1, 0), data = x,cond.dist="std", include.mean = FALSE, trace=F)
fit2

vol.est = fit1@h.t

vol.est[1000]

pred = predict(fit1,12)
pred


sigma.hat = fit1@h.t
pred.sigma = pred[,3]
pred.sigma

ts.plot(sigma.hat,lwd=3, xlim=c(0,2012))
lines(2001:2012,pred.sigma, col="red", lwd=3)
title( "PredicciÛn a 12 pasos", cex.main =2)

## zoom Predicciones:

ts.plot(sigma.hat[1800:2000],lwd=3, xlim=c(0,215))
lines(201:212,pred.sigma, col="red", lwd=3)
title( "Ultimos 200 valores de la serie y prediccion a 12 pasos")

#Simulacion con fGarch
library(fGarch)
set.seed(2000)
spec = garchSpec(model = list(omega= 0.3,alpha = c(0.7), beta = 0))

x=garchSim(spec, n = 2000)
