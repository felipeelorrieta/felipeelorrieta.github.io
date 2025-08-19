####Trends

t<-1:100
lt<-t+rnorm(100)
qt<-(t+rnorm(100))^2
logt<-log(t+rnorm(100))
et<-exp(t)+rnorm(100)
par(mfrow=c(2,2))
ts.plot(lt,main='Linear Trend')
ts.plot(qt,main='Quadratic Trend')
ts.plot(logt,main='Logarithmic Trend')
ts.plot(et,main='Exponential Trend')


###Regression

t<-1:length(co2)
Modelo<-lm(co2~t)
summary(Modelo)
ts.plot(residuals(Modelo))
plot(t,co2,type='l')
lines(t,fitted(Modelo),col='red')
t2<-t**2
Modelo2<-lm(co2~t+t2)
summary(Modelo2)
ts.plot(residuals(Modelo2))
plot(t,co2,type='l')
lines(t,fitted(Modelo2),col='red')
t3<-t**3
Modelo3<-lm(co2~t+t2+t3)
summary(Modelo3)
ts.plot(residuals(Modelo3))
plot(t,co2,type='l')
lines(t,fitted(Modelo3),col='red')
t4<-t**4
Modelo4<-lm(co2~t+t2+t3+t4)
summary(Modelo4)
ts.plot(residuals(Modelo))
plot(t,co2,type='l')
lines(t,fitted(Modelo),col='red')
lines(t,fitted(Modelo2),col='blue')
lines(t,fitted(Modelo3),col='green')


###Modelo Armónico

t<-1:100
ts.plot(20+10*sin((2*pi/24)*t+2*pi/24))

co2
ts.plot(co2)
t<-1:length(co2)
t2<-t**2
t3<-t**3
Modelo<-lm(co2~t+t2+t3)
summary(Modelo)
ts.plot(residuals(Modelo))
plot(t,co2,type='l')
lines(t,fitted(Modelo),col='red')
sum(residuals(Modelo)**2)

z<-residuals(Modelo)
ts.plot(z)
ts.plot(z[1:40])
ts.plot(z[1:25])
n<-length(z)

w<-1/12 #Frecuencia

ajuste<-lm(z~sin(2*pi*w*t)+cos(2*pi*w*t)-1)
summary(ajuste)

# raiz(A^2*seno(psi)^2+A^2*coseno(psi)^2)= Amplitud

sqrt(ajuste$coef[1]**2+ajuste$coef[2]**2)

# sin(psi)/coef(psi)=tan(psi) luego atan(psi) se obtiene el valor de psi

atan(ajuste$coef[1]/ajuste$coef[2])
ts.plot(z)
lines(ajuste$fit,lwd=2,col="red")
res<-ajuste$res
ts.plot(res)
msearm<-mean(res**2)
ts.plot(res)
ajuste2<-lm(z~sin(2*pi*w*t)+cos(2*pi*w*t)+sin(2*pi*2*w*t)+cos(2*pi*2*w*t)-1)
ts.plot(z)
lines(ajuste2$fit,lwd=2,col="red")
lines(ajuste$fit,lwd=2,col="blue")
res<-ajuste2$res
ts.plot(res)
msearm2<-mean(res**2)

ajuste3<-lm(z~sin(2*pi*w*t)+cos(2*pi*w*t)+sin(2*pi*2*w*t)+cos(2*pi*2*w*t)+sin(2*pi*3*w*t)+cos(2*pi*3*w*t)-1)
ts.plot(z)
lines(ajuste3$fit,lwd=2,col="green")
lines(ajuste2$fit,lwd=2,col="red")
lines(ajuste$fit,lwd=2,col="blue")
res<-ajuste3$res
ts.plot(res)


fin<-lm(co2~ t+t2+t3+sin(2*pi*w*t)+cos(2*pi*w*t)+sin(2*pi*2*w*t)+cos(2*pi*2*w*t))
summary(fin)
mean(residuals(fin)**2)
plot(t,co2,type='l')
lines(t,fitted(fin),col='red')


#Modelo Variables Dummy
n=length(co2)
Mes = as.factor(rep(1:12, n/12))
fit = lm(z ~  Mes)
summary(fit)
ts.plot(z)
lines(fit$fit,lwd=2,col="red")
lines(ajuste2$fit,lwd=2,col="blue")
res2<-fit$res
msedum=mean(res2**2)

msedum
msearm

par(mfrow=c(2,1))
plot(t,res2,type='l')
plot(t,res,type='l')

fin<-lm(co2~ t+t2+t3+Mes)
summary(fin)
mean(fin$res**2)

mean(residuals(fin)**2)
new<-cbind(469:568,(469:568)**2,(469:568)**3,as.factor(c(rep(1:12, 8),1,2,3,4)))
new2<-data.frame(t=new[,1],t2=new[,2],t3=new[,3],Mes=as.factor(new[,4]))
pr1<-predict(fin,new2,interval="prediction")
plot(1:568,seq(310,380,length=568),type='n')
lines(t,co2,col=1)
lines(469:568,pr1[,1],col='blue',lwd=2)
lines(469:568,pr1[,2],col='red',lwd=1)
lines(469:568,pr1[,3],col='red',lwd=1)

#White Noise
set.seed(1234)
par(mfrow=c(2,1))
x = e = rnorm(1000)
acf(x,lag=20,main="Uncorrelated Noise")
#Random Walk
for (t in 3:1000) x[t] = 0.3*x[t-2] + e[t]
acf(x,lag=20,main="Correlated Noise")

Box.test(e, lag = 2, type = c("Box-Pierce"))
Box.test(e, lag = 2, type = c("Ljung-Box"))

Box.test(x, lag = 1, type = c("Box-Pierce"))
Box.test(x, lag = 1, type = c("Ljung-Box"))

Box.Test = function(x, lag = 1, main = "p values for Ljung-Box statistic"){

B<-vector("numeric")
for(i in 1:lag){
B[i]<-Box.test(x, lag = i, type = "Ljung-Box")$p.value
}
A<-matrix(cbind(c(1:lag),B), nrow = lag, ncol = 2, byrow=F, dimnames = list(NULL, c("lag", "p.value")))
plot(A[,1], A[,2], ylim = c(0, 1), 
ylab = "p-value", xlab = "Lag", main = main)
abline(0.05, 0, col = 4, lty = 2)
return(A)
}

Box.Test(e,lag=20)
Box.Test(x,lag=20)

#Diferenciacion
par(mfrow=c(2,1))
y=diff(x)
acf(y)
acf(x,lag=20,main="Correlated Noise")
acf(y,lag=20,main="First Difference of Time Series")