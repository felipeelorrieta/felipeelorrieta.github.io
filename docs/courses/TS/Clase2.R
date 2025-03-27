x=c(105,110,107,112,118)
n<-length(x)
r<-2
m1<-rep(0,n-r)
m2<-rep(0,n-r)
m2[1]=x[1]
alpha=0.7
for(i in r:n)
{
	m1[i]<-mean(x[(i-1):i])
	m2[i]<-alpha*x[i]+(1-alpha)*m2[i-1]
}
plot(1:n,x,type='l')
lines(r:n,m1[-1],col='red',lwd=2)
lines(r:n,m2[-1],col='blue',lwd=2)
mse1=mean((x[-1]-m1[-1])**2)
mse2=mean((x[-1]-m2[-1])**2)
print(mse1)
print(mse2)

##MMS
require(astsa)
x<-unemp
n<-length(x)
r<-20
m1<-rep(0,n-r)
for(i in r:n)
{
	m1[i-r+1]<-mean(x[(i-(r-1)):i])
}
plot(1:n,x,type='l')
lines(r:n,m1,col='red',lwd=2)

####Simple Exponential Smoothing
require(astsa)
x<-unemp
temp.ts<-x
plot(temp.ts)
aliss<- HoltWinters(temp.ts,gamma=F,beta=F,alpha=0.1)
aliss
plot(aliss$x, main="Serie Original v/s Serie ajustada", xlab= "Tiempo", ylab="Observaciones/Predichos",type='l')
lines(time(temp.ts)[-length(x)],aliss$fitted[,2],col='red',lwd=2)
pred<- predict(aliss, 20, prediction.interval = TRUE)
a<-plot(aliss,pred, main="Serie Original v/s Serie ajustada y Horizonte de predicción", xlab= "Tiempo", ylab="Observaciones/Predichos")
aliss2<- HoltWinters(temp.ts,gamma=F,beta=F)
aliss2
lines(time(temp.ts)[-length(x)],aliss2$fitted[,2],col="blue")
sse<- aliss$SSE
sse
sse2<- aliss2$SSE
sse2
pred2<- predict(aliss2, 20, prediction.interval = TRUE)
a<-plot(aliss2,pred2, main="Serie Original v/s Serie ajustada y Horizonte de predicción", xlab= "Tiempo", ylab="Observaciones/Predichos")

###################################################
##### Alisado Exponencial Biparametrico de Holt ###
###################################################


ts.plot(austres)   
alisd1<- HoltWinters(austres, gamma=F)
alisd1
plot(alisd1, main="Serie Original v/s Serie ajustada", xlab= "Tiempo", ylab="Observaciones/Predichos",lwd=2)
pred1<- predict(alisd1, 20, prediction.interval = TRUE)
a<-plot(alisd1,pred1, main="Serie Original v/s Serie ajustada y Horizonte de predicción", xlab= "Tiempo", ylab="Observaciones/Predichos")
sse1<- alisd1$SSE
sse1

alisd2<- HoltWinters(austres, gamma=F,alpha=0.5)
alisd2
plot(alisd2, main="Serie Original v/s Serie ajustada", xlab= "Tiempo", ylab="Observaciones/Predichos")
pred2<- predict(alisd2, 20, prediction.interval = TRUE)
a<-plot(alisd2,pred1, main="Serie Original v/s Serie ajustada y Horizonte de predicción", xlab= "Tiempo", ylab="Observaciones/Predichos")
sse2<- alisd2$SSE
sse2

res1=alisd1$x-alisd1$fitted[,1]
res2=alisd2$x-alisd2$fitted[,1]
plot(as.numeric(res2),type="l")
lines(as.numeric(res1),col="red")

##############################################################################
##########################Metodo de Holt-Winters##############################
##############################################################################



##############################################################################
##############################Caso Aditivo####################################
##############################################################################

co2
ts.plot(co2)
plot(decompose(co2))
mean(na.omit(x$random)**2)
modadd=HoltWinters(co2,seasonal = "additive")
plot(modadd,main="Curva Original vs Curva Suavizada",ylab="Indice",xlab="Tiempo",col="blue",lwd=3)
predichos1<-fitted(modadd)
pred1<-predichos1[,1]
res1<-co2-pred
plot(res1)
modadd2=HoltWinters(co2,gamma=F,seasonal = "additive")
pred2<-fitted(modadd2)[,1]
res2<-co2-pred2
plot(res2)
lines(res1,col="blue")
a1<-modadd$alpha
b1<-modadd$beta
g1<-modadd$gamma
sse1<-modadd$SSE
p1 <- predict(modadd, 100, prediction.interval = TRUE)
plot(modadd, p1,lwd=2)
sse1
sse2<-modadd2$SSE
sse2



##############################################################################
###########################Caso Multiplicativo################################
##############################################################################

AirPassengers
ts.plot(AirPassengers)
plot(decompose(AirPassengers,type="multiplicative"))
x=decompose(AirPassengers)
mean(na.omit(x$random)**2)
modmult=HoltWinters(AirPassengers,seasonal = "multiplicative")
plot(modmult,main="Curva Original vs Curva Suavizada",ylab="Indice",xlab="Tiempo",col="blue",lwd=3)
predichos2<-fitted(modmult)
pred2<-predichos2[,1]
res2<-pred2-AirPassengers
a2<-modmult$alpha
b2<-modmult$beta
g2<-modmult$gamma
sse2<-modmult$SSE
p2 <- predict(modmult, 24, prediction.interval = TRUE)
plot(modmult, p2,main="Horizonte de Prediccion",ylab="Indice",xlab="Tiempo",col="blue",lwd=2)
sse2

