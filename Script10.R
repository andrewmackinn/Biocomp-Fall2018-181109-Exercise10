#read data
dat = read.table("data.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)
plot(dat$x, dat$y)
#for tidying dataframes
library(tidyr)

#like pt.1
nllike1= function(p,x,y){
  a =p[1]
  b=p[2]
  c=p[3]
  sigma = exp(p[4])
  expected = a+b*x+c*x^2
  nll1 = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
}
initialGuess = c(1,1,1,1)
fitComplex = optim(par = initialGuess, fn =nllike1, x = dat$x, y = dat$y)
print(fitComplex)


#like pt.2
nllike= function(p,x,y){
  a =p[1]
  b=p[2]
  sigma=exp(p[3])
  expected = a +b*x
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
}
initalGuess = c(7,25,4)
fitSimple = optim(par = initialGuess, fn =nllike, x = dat$x, y = dat$y)
print(fitSimple)
# get the p value            
teststat = 2* (fit$value - fitComplex$value)
df = length(fitComplex$par) - length (fitSimple$par)
p = 1 - pchisq(q = teststat, df=1)
p
# p >>> .05, so use simpler model

# modeling portion 
#ddSim function
ddSim = function(t,y,p){
  N1=y[1]
  R1=p[1]
  a11 = p[2]
  a12 = p[3]
  
  N2=y[2]
  R2=p[4]
  a22 = p[5]
  a21 = p[6]
  
  dN1dt=R1*(1-N1*a11-N2*a12)*N1
  dN2dt=R2*(1-N2*a22-N1*a21)*N2
  
  return(list(c(dN1dt, dN2dt)))
}
#pt.1
params=c(.5,.003,.001, .4,.003,.001)
NO=c(2,2)
times = 1:100

modelSim1 = ode(y=NO, times = times, func = ddSim, parms = params)
out = data.frame(data=modelSim1, time = modelSim1[,1], mod1=modelSim1[,2], mod2=modelSim1[,3])

out %>%
  gather(key,value, mod1, mod2) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line()

#pt.2
params=c(.5,.004,.001, .4,.004,.001)
NO=c(2,2)
times = 1:100

modelSim2 = ode(y=NO, times = times, func = ddSim, parms = params)
out2 = data.frame(data=modelSim2, time = modelSim2[,1], mod1B=modelSim2[,2], mod2B=modelSim2[,3])

out2 %>%
  gather(key,value, mod1B, mod2B) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line()


#pt.3
params=c(.4,.006,.004, .4,.006,.005)
NO=c(2,2)
times = 1:100

modelSim3 = ode(y=NO, times = times, func = ddSim, parms = params)
out3 = data.frame(data=modelSim3, time = modelSim3[,1], mod1C=modelSim3[,2], mod2C=modelSim3[,3])

out3 %>%
  gather(key,value, mod1C, mod2C) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line()

#expected = a +b*x +c*x^2
#r <1
#k = 1/alpha 
#alpha <.1