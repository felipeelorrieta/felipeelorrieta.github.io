AAPL=read.csv(file.choose())
AAPL=AAPL[-1,]
AAPL=AAPL[order(AAPL[,1]),]
AAPL = AAPL$close
n = length(AAPL)

plot(AAPL,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( "AAPL", cex.main =2)

AAPL.log = log(AAPL)
plot(AAPL.log,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( "log(AAPL)", cex.main =2)

AAPL.rtn=diff(log(AAPL)) 
plot(AAPL.rtn,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( "Retornos", cex.main =2)

x=AAPL.rtn
par(mfrow=c(2,1),cex=0.8)
acf(x, main="ACF Retornos", lwd=3)
pacf(x, lwd=3)

hist(x, main="Histograma Retornos", col="grey", border="white")
plot(density(x), main="DistribuciÛn Retornos", lwd=3)
qqnorm(x)
qqline(x)


## Retornos^2

xx = x^2

par(mfrow=c(2,1),cex=0.8)
plot(xx,bty="n" ,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( expression(Retornos^2), cex.main =2)
acf(xx, main="",lwd=3)
title(expression(paste("ACF " , Retornos^2)),cex.main=1.5)


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


par(mfrow=c(2,1),cex=0.8)
acf(x, main="ACF Retornos", lwd=3)
pacf(x)

hist(x, main="Histograma Retornos", col="grey", border="white")
plot(density(x), main="DistribuciÛn Retornos", lwd=3)
qqnorm(x)
qqline(x)


## Retornos^2

xx = x^2

par(mfrow=c(1,2),cex=0.8)
plot(xx,bty="n" , lwd=3,type="l" , ylab="", las=1 ,xlab="", xaxt="n")
title( expression(Retornos^2), cex.main =2)
axis(1, at = c(0, 250,500,750,1000,1250,1500,1750,2000), label = c(2006, 2007,2008,2009,2010,2011,2012,2013,2014))
acf(xx, main="",lwd=3)
title(expression(paste("ACF " , Retornos^2)),cex.main=1.5)