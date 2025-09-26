library(fracdiff)
library(forecast)
library(arfima)
set.seed(123)
n <- 5000
arma.sim <- arima.sim(model = list(ar = 0.7), n = n)
d <- 0.40
arfima.sim <- fracdiff.sim(n, d = d)$series
lag.max <- 100
par(mfrow=c(1,3))
acf.arma <- acf(arma.sim, lag.max = lag.max,main="ACF ARMA")$acf[-1]
acf.arfima <- acf(arfima.sim, lag.max = lag.max,main="ACF ARFIMA")$acf[-1]
plot(1:lag.max, acf.arma, type = "l", col = "blue", lwd = 2,
     ylim = c(-0.1,1), ylab = "ACF", xlab = "Rezago",
     main = "Decaimiento de la autocorrelación")
lines(1:lag.max, acf.arfima, col = "red", lwd = 2)
legend("topright", legend = c("ARMA(1,0) = Exponencial", 
                              "ARFIMA(0,d,0) = Hiperbólico"),
       col = c("blue","red"), lwd = 2, bty = "n")

set.seed(3313)
d = 0.40 
X = fracdiff.sim(4000, d = d)$series
par(mfrow = c(1,2), bty = "n", las = 1)
plot(X, type = "l", bty = "n", las = 1, col = "gray", ylab = "")
plot(X, type = "l", bty = "n", las = 1, xlim = c(3500,4000), col = "gray", ylab = "")

par(mfrow = c(1,2), bty = "n", las = 1)
acf(X, lag.max = 100, ylim = c(-1,+1), main = "")
pacf(X, lag.max = 100, ylim = c(-1,+1), main = "")

set.seed(3313)
par(mfrow = c(2,3))
d = 0.40 
X = fracdiff.sim(4000, ar=0.7 , d = d)$series
plot(X, type = "l", bty = "n", las = 1, col = "gray", ylab = "",main="d=0.4")
d = 0.0 
X1 = fracdiff.sim(4000, ar=0.7 , d = d)$series
plot(X1, type = "l", bty = "n", las = 1, col = "gray", ylab = "",main="d=0.0")
d = -0.4 
X2 = fracdiff.sim(4000, ar=0.7 , d = d)$series
plot(X2, type = "l", bty = "n", las = 1, col = "gray", ylab = "",main="d=-0.4")
acf(X, lag.max = 100, ylim = c(-1,+1), main = "")
acf(X1, lag.max = 100, ylim = c(-1,+1), main = "")
acf(X2, lag.max = 100, ylim = c(-1,+1), main = "")


fit_1 <- forecast::auto.arima(X, d = 1)
summary(fit_1)
## ARMA
fit_2 <- forecast::auto.arima(X, d = 0, allowmean = F)
summary(fit_2)

plot(fit_2)

par(mfrow = c(1,1))
acf(X, lag.max = 100)
ACF <- ARMAacf(ar = fit_2$coef[1:5], ma = NULL, lag.max = 100)
lines(ACF ~ c(0:100), type = "p", col = "red", pch = 20)

fit_3 = fracdiff(X,drange=c(0,0.5))

par(mfrow = c(1,1), bty = "n", las = 1)
acf(X, lag.max = 100, ylim = c(-1,+1), main = "")
d <- fit_3$d
d <- 0.41
k <- 1:100
ACF <- gamma(1-d)*gamma(k+d) / (gamma(d)*gamma(1+k-d))
lines(c(1,ACF) ~ c(0:100), type = "p", col = "red", pch = 20)

summary(fit_3) 

plot(X, type = "l", col = "gray")
lines(fit_2$fitted, col = "blue")
lines(fit_3$fitted, col = "red")
