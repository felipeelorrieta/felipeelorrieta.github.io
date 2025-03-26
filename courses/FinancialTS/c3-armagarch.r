##################
## Ejemplo IPSA ##
##################

datos = read.table(file.choose(),header=T)
attach(datos)

ipsa = na.omit(datos[,1])
n = length(ipsa)

plot(ipsa,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( "IPSA", cex.main =2)
axis(1, at = c(0, 250,500,750,1000,1250,1500,1750,2000), label = c(2006, 2007,2008,2009,2010,2011,2012,2013,2014))

y = log(ipsa)

plot(y,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( "log(IPSA)", cex.main =2)
axis(1, at = c(0, 250,500,750,1000,1250,1500,1750,2000), label = c(2006, 2007,2008,2009,2010,2011,2012,2013,2014))


## Retornos:

x = diff(y)
plot(x,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( "Retornos", cex.main =2)
axis(1, at = c(0, 250,500,750,1000,1250,1500,1750,2000), label = c(2006, 2007,2008,2009,2010,2011,2012,2013,2014))

t.test(x)


par(mfrow=c(2,1),cex=0.8)
acf(x, main="ACF Retornos", lwd=3)
pacf(x,lwd=3)

hist(x, main="Histograma Retornos", col="grey", border="white")
plot(density(x), main="Distribucion Retornos", lwd=3)
qqnorm(x)
qqline(x)


## Retornos^2

xx = x^2

par(mfrow=c(2,1),cex=0.8)
plot(xx,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( expression(Retornos^2), cex.main =2)
axis(1, at = c(0, 250,500,750,1000,1250,1500,1750,2000), label = c(2006, 2007,2008,2009,2010,2011,2012,2013,2014))
acf(xx, main="",lwd=3)
title(expression(paste("ACF " , Retornos^2)),cex.main=1.5)

Box.Test = function(x, lag = 1, main = "p values for Ljung-Box statistic"){

B<-vector("numeric")
for(i in 1:lag){
B[i]<-Box.test(x, lag = i, type = "Ljung-Box")$p.value
}
A<-matrix(cbind(c(1:lag),B), nrow = lag, ncol = 2, byrow=F, dimnames = list(NULL, c("lag", "p.value")))
plot(A[,1], A[,2], ylim = c(0, max(0.051,(max(A[,2])+.01))), 
ylab = "p-value", xlab = "Lag", main = main)
abline(0.05, 0, col = 4, lty = 2)
return(A)
}

par(mfrow=c(2,1))
Box.Test(x,40, main= expression(paste(" valores-p Box Ljung ", hat(e[t] ))))
Box.Test(x^2,40, main= expression(paste(" valores-p Box Ljung ",  hat(e[t]^2 ))))

par(mfrow=c(2,1))
acf(x^2, lwd=3, main ="")
pacf(x^2,main="", lwd=3)

par(mfrow=c(2,1))
acf(x, lwd=3, main =expression(paste("ACF Retornos ",r[t])))
acf(x^2,main=expression(paste("ACF Retornos Cuadrados ",r[t]^2)), lwd=3)


###########################
## Ajuste modelo ARCH(1) ##
###########################
require(TSA)
McLeod.Li.test(y=x)
require(rugarch)
spec1 <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,0), include.mean=FALSE),dist="std")
fit1 <- ugarchfit(spec1, x)

nu = fit1@fit$coef[4]
nu

as.numeric(residuals(fit1))/as.numeric(sigma(fit1))
resid.rugarch <- as.numeric(residuals(fit1, standardize=TRUE))
ts.plot(residuals(fit1))
ts.plot(resid.rugarch)

pit=pit(fit1) 
hist(pit) # this should be approximately Uniform[0,1]
norm=qnorm(pit)
hist(norm) # this should be approximately standard normal
qqnorm(norm); abline(a=0, b=1)

show(fit1)
v1=sigma(fit1)
ts.plot(as.numeric(v1))
summary(v1-pred10[,1])
coef(fit1)
infocriteria(fit1) #AIC
head(as.numeric(sigma(fit1)))**2 #Varianza Estimada
ts.plot(as.numeric(sigma(fit1)),ylab="",main='Volatilidad Estimada')

head(as.numeric(fitted(fit1)))  #Media Estimada
plot(fit1)

forc = ugarchforecast(fit1, n.ahead=12)
plot(forc)

###IGARCH

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0)))
fit4=ugarchfit(data=sp5,spec=spec1)
show(fit4)

v1=sigma(fit4)
ts.plot(as.numeric(v1))  ### Not shown.
### You can also obtain various plots
plot(fit4) ## There are 12 choices. 
infocriteria(fit4)

######### ARCH-M model with volatility in mean
spec2=ugarchspec(variance.model=list(model="sGARCH"),
                 mean.model=list(armaOrder=c(0,0),archm=TRUE)) #Difference ARCH IN MEAN archm=TRUE
fit5=ugarchfit(data=sp5,spec=spec2)
fit5
infocriteria(fit5)

