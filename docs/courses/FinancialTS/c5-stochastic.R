set.seed(6713)
require("stochvol")
sim = svsim(1000,mu=-10,phi=0.99,sigma=0.2)
ts.plot(sim$y)
require("rugarch")
spec1 <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,0), include.mean=FALSE),dist="std")
fit1 <- ugarchfit(spec1, sim$y)
#plot(fit1)

model=svsample(sim$y,draws=10000)
summary(model)
plot(model)
vol=model$summary$latent
myresid <- resid(model) 
plot(myresid, sim$y)
par(mfrow=c(2,2))
Box.Test = function(x, lag = 1, main = "p values for Ljung-Box statistic"){

B<-vector("numeric")
for(i in 1:lag){
B[i]<-Box.test(x, lag = i, type = "Ljung-Box")$p.value
}
A<-matrix(cbind(c(1:lag),B), nrow = lag, ncol = 2, dimnames = list(NULL, c("lag", "p.value")))
plot(A[,1], A[,2], ylim = c(0, 1), 
ylab = "p-value", xlab = "Lag", main = main,lwd=2)
abline(0.05, 0, col = 4, lty = 2,lwd=2)
return(A)
}
x=sim$y
acf(x, lwd=3, main ="")
title(expression(paste("ACF ", e[t])))
acf(x^2,main="", lwd=3)
title(expression(paste("ACF ", e[t]^2)))
Box.Test(x,20, main= expression(paste(" valores-p Box Ljung ", e[t])))
Box.Test(x^2,20, main= expression(paste(" valores-p Box Ljung ", e[t]^2)))

x=as.numeric(myresid)
acf(x, lwd=3, main ="")
title(expression(paste("ACF ", e[t])))
acf(x^2,main="", lwd=3)
title(expression(paste("ACF ", e[t]^2)))
Box.Test(x,20, main= expression(paste(" valores-p Box Ljung ", e[t])))
Box.Test(x^2,20, main= expression(paste(" valores-p Box Ljung ", e[t]^2)))

fore <- predict(model, 20)
 
## plot the results
plot(model, forecast = fore)


setwd("/Users/felocarril/Dropbox/Econometría/1-2023/Codigos R")
sp500= read.table("sp500.txt")
rtn=as.numeric(sp500[,1])
ts.plot(rtn)
par(mfrow=c(2,1))
acf(rtn)
pacf(rtn)
par(mfrow=c(2,2))
hist(rtn, main="Histograma Retornos", col="grey", border="white")
plot(density(rtn), main="Distribucion Retornos", lwd=3)
par(mfrow=c(2,1))
ts.plot(rtn,ylab=expression(r[t]))
qqnorm(rtn)
qqline(rtn)

library("moments")
skewness(rtn)
kurtosis(rtn)

model=svsample(rtn,draws=10000)
summary(model)
plot(model)
vol=model$summary$latent
myresid <- resid(model) 
plot(myresid, sim$y)
par(mfrow=c(2,2))
x=as.numeric(myresid)
acf(x, lwd=3, main ="")
title(expression(paste("ACF ", e[t])))
acf(x^2,main="", lwd=3)
title(expression(paste("ACF ", e[t]^2)))
Box.Test(x,20, main= expression(paste(" valores-p Box Ljung ", e[t])))
Box.Test(x^2,20, main= expression(paste(" valores-p Box Ljung ", e[t]^2)))

model=svtsample(rtn,draws=10000,thin=100,burnin=3000)
summary(model)
plot(model)
vol=model$summary$latent
myresid <- resid(model) 
plot(myresid, sim$y)
par(mfrow=c(2,2))
x=as.numeric(myresid)
acf(x, lwd=3, main ="")
title(expression(paste("ACF ", e[t])))
acf(x^2,main="", lwd=3)
title(expression(paste("ACF ", e[t]^2)))
Box.Test(x,20, main= expression(paste(" valores-p Box Ljung ", e[t])))
Box.Test(x^2,20, main= expression(paste(" valores-p Box Ljung ", e[t]^2)))


set.seed(123)
N <- 100
x <- rnorm(N, 0, 1)
y <- 3 + 2 * x + rnorm(N, 0, 1)

# Lista para Stan
stan_data <- list(
  N = N,
  x = x,
  y = y
)

library(rstan)

# Compilar el modelo
setwd("/Users/felocarril/Dropbox/")
stan_model <- stan_model(file = "regresion_model.stan.R")
