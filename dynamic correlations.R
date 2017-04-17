install.packages("quantmod")
rm(list=ls())

#step a
library('quantmod')
getSymbols('VIIIX',from='1998-1-1', to='2017-4-7')
ret1<-diff(log(VIIIX$VIIIX.Adjusted))
ind<- which(ret1!=0)                        
ret<- ret1[ind]
getSymbols('VGTSX',from='1998-1-1', to='2017-4-7')
ret2<-diff(log(VGTSX$VGTSX.Adjusted))
ind1<- which(ret2!=0)                     
ret3<- ret2[ind]
T <- length(ret1)
plot(ret, type='l', lwd=2, col='blue')
points(ret3, type='l', lwd=2, col='red')

#step b
library('fGarch')
fit=garchFit(formula = ~garch(1,1), data=ret, trace = FALSE)
sigma <- sqrt(fit@h.t)
retstand <- ret/sigma
fit3=garchFit(formula = ~garch(1,1), data=ret3, trace = FALSE)
sigma3<- sqrt(fit3@h.t)
ret3stand <- ret3/sigma3
lambda<- 0.94
q11<- numeric(T)
q12<- numeric(T)
q22<- numeric(T)
for (i in 2:T){
  q11[i] <- (1-lambda)*retstand[i-1]^2 + lambda*q11[i-1]
  q12[i] <- (1-lambda)*retstand[i-1]*ret3stand[i-1] + lambda*q12[i-1]
  q22[i] <- (1-lambda)*ret3stand[i-1]^2 + lambda*q22[i-1]
}
exponentialCorr <- q12/sqrt(q11*q22)
plot(exponentialCorr, type='l', col='blue', lwd=4)

#step c
alpha<- 0.05
beta<- 0.9
q11<- numeric(T)
q12<- numeric(T)
q22<- numeric(T)
q11lr<- mean(retstand^2)
q12lr<- mean(retstand*ret3stand)
q22lr<- mean(ret3stand^2)
for (i in 2:T){
  q11[i] <- q11lr + alpha*(retstand[i-1]^2 - q11lr) + beta*(q11[i-1]-q11lr)
  q12[i] <- q12lr + alpha*(retstand[i-1]*ret3stand[i-1] - q12lr) + beta*(q12[i-1]-q12lr)
  q22[i] <- q22lr + alpha*(ret3stand[i-1]^2 - q22lr) + beta*(q22[i-1]-q22lr)
}
GarchCorr <- q12/sqrt(q11*q22)
plot(GarchCorr, type='l', col='blue', lwd=4)
points(exponentialCorr, type='l', col='red', lwd=4)

#step d need to write down anaylysis 

