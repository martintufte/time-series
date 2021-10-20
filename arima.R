library(forecast)
library(tscourse)

### DATA PREP ###

df <- read.csv(file = "data/CBBTCUSD.csv")

# remove earlier dates than 2015-01-19 (as they contain lots of NA)
df <- df[as.Date(df$DATE) >= as.Date("2015-01-19"),]
# fix NAN at "2020-09-04"
df[df$DATE=="2020-09-04",'CBBTCUSD'] = 10493.77
# convert to numeric values
df$CBBTCUSD <- as.numeric(df$CBBTCUSD)
# number of observations

ts <- as.ts(df$CBBTCUSD)
ts.log <- log(ts)
ts.diff <- diff(ts)
ts.log.diff <- diff(ts.log)

n <- length(ts)

p.hat <- c(0:5)
q.hat <- c(0:5)

AIC <- BIC <- matrix(NA,length(p.hat),length(q.hat))
for (i in 1:length(p.hat)) {
  for (j in 1:length(q.hat)) {
    try ({
      arima.out <- arima(ts.log,order=c(p.hat[i],1,q.hat[j]),include.mean = FALSE)
      AIC[i,j] <- arima.out$aic
      BIC[i,j] <- BIC(arima.out)
    })
  }
}

min.AIC = min(AIC)
which.AIC <- which(AIC == min.AIC,arr.ind = TRUE)
p.AIC <- p.hat[which.AIC[1]]
q.AIC <- q.hat[which.AIC[2]]

arima.out.AIC <- arima(ts.log,order = c(p.AIC,1,q.AIC))

# residuals
par(mfrow=c(2,2))
plot(arima.out.AIC$residuals)
qqnorm(arima.out.AIC$residuals)
qqline(arima.out.AIC$residuals)

plot(sacf(arima.out.AIC$residuals, max.lag=10)$rho.hat, type='o', xlab='lag',ylab='sacf')
hist(arima.out.AIC$residuals)

# white noice test
WNtest.LB(arima.out.AIC$residuals, k=p.AIC+q.AIC+1, nparms = p.AIC + q.AIC)

# For BIC

which.BIC <- which(BIC == min(BIC),arr.ind = TRUE)
p.BIC <- which.BIC[1]
q.BIC <- which.BIC[2]

arima.out.BIC <- arima(ts.log,order = c(p.BIC,1,q.BIC))

# residuals
par(mfrow=c(2,2))
plot(arima.out.BIC$residuals)
qqnorm(arima.out.BIC$residuals)
qqline(arima.out.BIC$residuals)
plot(sacf(arima.out.BIC$residuals, max.lag=10)$rho.hat, type='o', xlab='lag',ylab='sacf')

# make predictions
# AIC

h = 365
start_plot = length(ts.log)-h # plotting last h values + next h predictions
par(mfrow = c(2,1))

{
plot(ts.log,xlim=c(start_plot,n+h),ylim=c(8,13),main="Model with best AIC")

pred.arima.out.AIC <- predict(arima.out.AIC,n.ahead = h)
X.hpred.arima.AIC <- pred.arima.out.AIC$pred
lines((n+1):(n+h),X.hpred.arima.AIC,col="red")

# prediction interval:
X.hpred.se.arima.AIC <- pred.arima.out.AIC$se
lines(X.hpred.arima.AIC + 1.96 * X.hpred.se.arima.AIC ~ c((n+1):(n+h)),col="blue",lty=3 )
lines(X.hpred.arima.AIC - 1.96 * X.hpred.se.arima.AIC ~ c((n+1):(n+h)),col="blue",lty=3 )

## Extra: What about bootstrapping? Sometimes it can be useful!
B <- 1000

residuals <- c(arima.out.AIC$residuals)
Ysim <- matrix(nrow=B,ncol=h)

#Simulate B possible futures
for(i in 1:B){
  Ysim[i,] <- simulate(arima.out.AIC, nsim=h)
}

apply(Ysim,2,sd) #Very similar to X.hpred.se.arima.BIC
boot.forecast <- apply(Ysim,2,quantile,c(0.025,0.975))
lines(boot.forecast[1,] ~ c((n+1):(n+h)),col="blue",lwd=2)
lines(boot.forecast[2,] ~ c((n+1):(n+h)),col="blue",lwd=2)

## Plotting 5 out of the B possible futures simulated 
for(i in 1:5){
  lines(Ysim[i,] ~ c((n+1):(n+h)),col=i+1)
}
}


# predictions BIC
{
pred.arima.out.BIC <- predict(arima.out.BIC,n.ahead = h)
X.hpred.arima.BIC <- pred.arima.out.BIC$pred

plot(ts.log,xlim=c(start_plot,n+h),ylim=c(8,13), main="Model with best BIC")

lines((n+1):(n+h),X.hpred.arima.BIC,col="red")

# prediction interval:
X.hpred.se.arima.BIC <- pred.arima.out.BIC$se
lines(X.hpred.arima.BIC + 1.96 * X.hpred.se.arima.BIC ~ c((n+1):(n+h)),col="blue",lty=3 )
lines(X.hpred.arima.BIC - 1.96 * X.hpred.se.arima.BIC ~ c((n+1):(n+h)),col="blue",lty=3 )


## Bootstrapping
B <- 1000

residuals <- c(arima.out.BIC$residuals)
Ysim <- matrix(nrow=B,ncol=h)

#Simulate B possible futures
for(i in 1:B){
  Ysim[i,] <- simulate(arima.out.BIC, nsim=h)
}

apply(Ysim,2,sd) #Very similar to X.hpred.se.arima.BIC
boot.forecast <- apply(Ysim,2,quantile,c(0.025,0.975))
lines(boot.forecast[1,] ~ c((n+1):(n+h)),col="black",lwd=2)
lines(boot.forecast[2,] ~ c((n+1):(n+h)),col="black",lwd=2)

## Plotting 5 out of the B possible futures simulated 
for(i in 1:5){
  lines(Ysim[i,] ~ c((n+1):(n+h)),col=i+1)
}
}

