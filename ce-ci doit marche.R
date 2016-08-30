
#j'essaie le garch model, j'espere que ca marche

rm(list=ls())
download.packages()
require(xlsx)
require(tseries)
require(fGarch)

#reading the data

dailydata <- read.csv(file.Marketpf19262014_daily.csv,1)
dailydata<-read.csv(file.choose(),header= T)
dailydata


vR=as.matrix(dailydata[2:nrow(dailydata),2])
vR

#out= garch(vR) # estimates a GARCH(1,1) model by default. a0 corresponds to w or omega, a1 corresponds to alpha, b1 corresponds to beta
#summary(out)
# this procedure is very simple and fast, but it has very limited tweaking abilities. Also the JB test is not calculated for the standardized residuals but the actual residuals. Testing whether these are normal makes no sense.
# it however refused to run so tried a new one

# Better estimation method (Requires package fGarch) 
out=garchFit(formula=~garch(1,1),data=vR)

summary(out)
vparms=coef(out)

# Calculating the standardized residuals manually to see if we get the same thing.
muhat=vparms[1]
omegahat=vparms[2]
alphahat=vparms[3]
betahat=vparms[4]
sigma2=sd(vR)^2 # setting the starting value of the sigma^2 process equal to the sample variance
veps=matrix(0,nrow(vR),1)
for ( t in 1:nrow(vR)){
  eta=vR[t]-muhat
  veps[t]=eta/sqrt(sigma2)
  sigma2=omegahat + betahat*sigma2 + eta^2*alphahat
}
#jarque.bera.test(veps) # the values are close enough.
jarqueberaTest(veps)

# you can also get the standardized residuals out by using the function residuals with the option standardize
veps_=residuals(out,standardize=TRUE)
jarqueberaTest(veps_) # the values are close enough.

# if you want we can also estimate the GJR model using garchFit
# the gamma parameter corresponds to what we call gamma in the slides
out=garchFit(formula=~garch(1,1),data=vR,leverage=TRUE) 
summary(out)
vparms=coef(out)



# Let us try and simulate from a GARCH(1,1) model and see if we can recover the parameters

alpha=0.06
beta=0.8 # high persistence in vol
w=0.2^2*(1-alpha-beta)/250 # setting w so that long term annual vol is roughly 20%
mu=0.10/250 # 10% annual expected return
T=1000 # roughly 4 years of daily data
vsigma2=matrix(0,T+1,1)
vsigma2[1]=0.20^2/250
vR_mc = matrix(0,T,1)

for (t in 1:T) { eta =rnorm(1,mean=0,sd=sqrt(vsigma2[t,1])) 
                 vR_mc[t,1] =mu + eta
                 vsigma2[t+1,1]= w +beta*vsigma2[t,1] + alpha*eta^2
}

out_mc=garchFit(formula=~garch(1,1),data=vR_mc) 
summary(out_mc)
