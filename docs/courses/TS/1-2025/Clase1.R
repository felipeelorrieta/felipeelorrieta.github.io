####Tipos de Series de Tiempo
ts.plot(sunspot.year)
ts.plot(window(sunspot.year, 1942,1988))
ts.plot(uspop,xlab = "Year",ylab = "U.S. Population (millions)")
segments(0,uspop[9], time(uspop)[9], uspop[9], col= 2,lty=2)
segments(time(uspop)[9],-10,time(uspop)[9], uspop[9], col= 2,lty=2)
segments(0,uspop[19], time(uspop)[19], uspop[19], col= 2,lty=2)
segments(time(uspop)[19],-10,time(uspop)[19], uspop[19], col= 2,lty=2)
require(astsa)
ts.plot(unemp,ylab='Unemployment Rate in USA')
data()

####Autocorrelation Function
par(mfrow=c(2,2))
y<-rnorm(1000,0,5) #Gaussian White Noise
x<-acf(y,lag.max=20)
x$acf
x<-acf(y,type='covariance')
x$acf

x<-acf(unemp,lag.max=70)
x$acf

#Random Walk
par(mfrow=c(2,2))
y<-rnorm(1000,0,5) #Gaussian White Noise
x<-acf(y,lag.max=20,main="")
x=rep(rnorm(1,0,1),1000)
e=x
for (t in 2:1000) x[t] = x[t-1] + e[t]
acf(x,lag=20,main="")
x=rep(rnorm(1,0,1),1000)
e=x
for (t in 2:1000) x[t] = 0.6*x[t-1] + e[t]
acf(x,lag=20,main="")
x=rep(rnorm(1,0,1),1000)
e=x
for (t in 2:1000) x[t] = -0.6*x[t-1] + e[t]
acf(x,lag=20,main="")


##Decompose Function

plot(decompose(co2))
x<-decompose(AirPassengers,type='multiplicative')
plot(x)

ts.plot(co2)
ts.plot(AirPassengers)
ts.plot(sunspot.year)
ts.plot(uspop)











