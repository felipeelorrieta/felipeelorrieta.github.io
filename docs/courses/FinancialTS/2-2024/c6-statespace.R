require("dlm")
AR2=dlmModARMA(ar = c(.45,-.65), sigma2=1)
AR2
Mod(eigen(AR2$GG)$values)

ARMA1=dlmModARMA(ar = c(.5),ma=c(.5), sigma2=1)
ARMA1
eigen(ARMA1$GG)$values

serie<-arima.sim(n=1000,model=list(ar = c(.5,.1),sd=1))
AR2=dlmModARMA(ar = c(.5,.1), sigma2=1)
AR2
buildARMA <- function(parm) {
  ar <- parm[1]  
  ar2 <- parm[2]  
  v <- parm[3]  
  dlmModARMA(ar = c(ar,ar2), sigma2 = v)
}
initial_guess <- c(0.5, 0.1, 1)

arma_fit <- dlmMLE(y = serie, parm = initial_guess, build = buildARMA)
arma_fit$par 
fitted_model <- buildARMA(arma_fit$par)

cat("Parámetros estimados: AR1 =", arma_fit$par[1], 
    "AR2 =", arma_fit$par[2], 
    "Varianza =", arma_fit$par[3], "\n")

require(FKF)
ans <- FKF::fkf(a0 = rep(0,2), P0 = diag(2), dt = matrix(0,2,1),
                  ct = 0, Tt = AR2$GG, Zt = AR2$FF,
                  HHt = AR2$W, GGt = AR2$V,
                  matrix(serie, nrow = 1))
names(ans)
ans$logLik
fitted=AR2$FF%*%ans$at[,-1]
MSE=mean((serie-fitted[1,])**2)

phi1=seq(-1,1,length=101)
phi2=seq(-1,1,length=101)
GG=AR2$GG
ans=matrix(-1e10,101,101)
for(i in 1:101)
{
	for(j in 1:101)
	{
		GG=AR2$GG
		GG[,1]=c(phi1[i],phi2[j])
		vp=round(abs(eigen(GG)$values),4)
		if(all(vp<1))
		ans[i,j] <- FKF::fkf(a0 = rep(0,2), P0 = diag(2), dt = matrix(0,2,1),
                  ct = 0, Tt = GG, Zt = AR2$FF,
                  HHt = AR2$W, GGt = AR2$V,
                  matrix(serie, nrow = 1))$logLik	
	}
}
col=which.max(apply(ans,1,max))
row=which.max(apply(ans,2,max))
phi1[col]
phi2[row]
max(ans)
arima(serie,order=c(2,0,0))

#ARMA11
serie<-arima.sim(n=1000,model=list(ar = c(.5),ma = c(.5)))
ans <- FKF::fkf(a0 = rep(0,2), P0 = diag(2), dt = matrix(0,2,1),
                  ct = 0, Tt = ARMA1$GG, Zt = ARMA1$FF,
                  HHt = ARMA1$W, GGt = ARMA1$V,
                  matrix(serie, nrow = 1))
names(ans)
ans$logLik
fitted=ARMA1$FF%*%ans$at[,-1]
MSE=mean((serie-fitted[1,])**2)

phi1=seq(-1,1,length=101)
theta1=seq(-1,1,length=101)
GG=ARMA1$GG
ans=matrix(-1e10,101,101)
for(i in 1:101)
{
	for(j in 1:101)
	{
		GG=ARMA1$GG
		GG[1,1]=c(phi1[i])
		W=matrix(c(1,theta1[j],theta1[j],theta1[j]**2),2,2)
		vp=round(abs(eigen(GG)$values),4)
		if(all(vp<1))
		ans[i,j] <- FKF::fkf(a0 = rep(0,2), P0 = diag(2), dt = matrix(0,2,1),
                  ct = 0, Tt = GG, Zt = ARMA1$FF,
                  HHt = W, GGt = ARMA1$V,
                  matrix(serie, nrow = 1))$logLik	
	}
}
col=which.max(apply(ans,1,max))
row=which.max(apply(ans,2,max))
phi1[col]
theta1[row]
max(ans)
arima(serie,order=c(1,0,1))

set.seed(6713)
require("stochvol")
sim = svsim(1000,mu=-10,phi=0.99,sigma=0.2)
ans <- FKF::fkf(a0 = -1, P0 = diag(1), dt = matrix(-10,1,1),
                  ct = 0, Tt = matrix(0.99), Zt = matrix(1),
                  HHt = matrix(0.2), GGt = matrix(pi**2/2),
                  matrix(sim$y, nrow = 1))
ans$logLik
fitted=exp(matrix(1)%*%ans$at[,-1])
MSE=mean((serie-fitted[1,])**2)
