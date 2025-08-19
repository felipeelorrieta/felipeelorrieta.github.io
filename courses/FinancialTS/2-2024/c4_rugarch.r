setwd('/Users/felocarril/Dropbox/Econometría/2-2024/Codigos R')
sp5=scan("sp500.txt")
ts.plot(sp5) #returns ofStandard & Poor's 500 (Standard & Poor's 500 Index) 
par(mfrow=c(2,1))
acf(sp5)
pacf(sp5)
par(mfrow=c(2,1))
acf(sp5**2)
pacf(sp5**2)

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
Box.Test(sp5,40, main= expression(paste(" valores-p Box Ljung ", hat(e[t] ))))
Box.Test(sp5^2,40, main= expression(paste(" valores-p Box Ljung ",  hat(e[t]^2 ))))

require(rugarch)
#### Specify a standard GARCH(1,1) model with mean-equation being a constant.
spec1=ugarchspec(variance.model=list(model="sGARCH"),
        mean.model=list(armaOrder=c(0,0)))
fit1=ugarchfit(data=sp5,spec=spec1)
show(fit1)
v1=sigma(fit1)
ts.plot(as.numeric(v1))
coef(fit1)
infocriteria(fit1) #AIC
head(as.numeric(sigma(fit1)))**2 #Varianza Estimada
head(as.numeric(fitted(fit1)))  #Media Estimada
head(as.numeric(residuals(fit1))) #Residuos

plot(fit1)

forc = ugarchforecast(fit1, n.ahead=12)
plot(forc)

##ARMA-GARCH###
spec1=ugarchspec(variance.model=list(model="sGARCH"),
        mean.model=list(armaOrder=c(1,1), include.mean = TRUE),
distribution.model = "norm")
fit3=ugarchfit(data=sp5,spec=spec1)
coef(fit3)


forc = ugarchforecast(fit3, n.ahead=12)
plot(forc)

##T-student innovations

spec1=ugarchspec(variance.model=list(model="sGARCH"),
        mean.model=list(armaOrder=c(1,1), include.mean = TRUE),
distribution.model = "std")
fit3=ugarchfit(data=sp5,spec=spec1)
coef(fit3)



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
