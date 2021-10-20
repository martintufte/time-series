source('functions/sacf.R')
source('functions/pacf.R')
source('functions/DurbinLevinson.R')
source('functions/innovations.R')
source('functions/filters.R')
library(stats)
library(forecast)
library(tscourse)

### DATA PREP ###

df <- read.csv(file = "data/CBBTCUSD.csv")

# remove earlier dates than 2015-01-19 (as they contain lots of NA)
df <- df[as.Date(df$DATE) >= as.Date("2015-01-19"),]
# fix NAN at "2020-09-04"
df[df$DATE=="2020-09-04",'CBBTCUSD'] = 10493.77
df$CBBTCUSD <- as.numeric(df$CBBTCUSD)
n <- dim(df)[1]
# change name of column to X
names(df)[names(df) == "CBBTCUSD"] <- "X"
# t is time after Bitcoins creation
df['t'] <- 1:n + 2206

df['X.log'] <- log(df$X)

X <- df$X
X.log <- df$X.log
X.diff <- diff(df$X)
X.log.diff <- diff(df$X.log)

n <- length(ts)

### Remove seasonality and trend

d <- 1 # no seasonality
s <- seasonality_estimator(df$X.log, d)
df['s'] <- rep(s, ceiling(n/d))[1:n]

fit <- lm(X.log ~ 1 + t, data = df) # linear trend beta0 + beta1 * t
df['m'] <- fit$fitted.values
beta0 <- fit$coefficients[1]
beta1 <- fit$coefficients[2]

# Add the stationary time series (Y = X.log - m - s)
df['Y'] = df$X.log - df$m - df$s


# find best models with AIC and BIC
p.hat <- c(0:5)
q.hat <- c(0:5)


AIC <- BIC <- matrix(NA,length(p.hat),length(q.hat))
for (i in 1:length(p.hat)) {
  for (j in 1:length(q.hat)) {
    try ({
      arima.out <- arima(df$Y,order=c(p.hat[i],0,q.hat[j]),include.mean = FALSE)
      AIC[i,j] <- arima.out$aic
      BIC[i,j] <- BIC(arima.out)
    })
  }
}

min.AIC = min(AIC)
which.AIC <- which(AIC == min.AIC,arr.ind = TRUE)
p.AIC <- p.hat[which.AIC[1]]
q.AIC <- q.hat[which.AIC[2]]

arima.out.AIC <- arima(df$Y,order = c(p.AIC,0,q.AIC))

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

arima.out.BIC <- arima(df$Y,order = c(p.BIC,1,q.BIC))

# residuals
par(mfrow=c(2,2))
plot(arima.out.BIC$residuals)
qqnorm(arima.out.BIC$residuals)
qqline(arima.out.BIC$residuals)
plot(sacf(arima.out.BIC$residuals, max.lag=10)$rho.hat, type='o', xlab='lag',ylab='sacf')

# make predictions
# AIC
{
h = 365
n = length(df$Y)
start_plot = n-h # plotting last h values + next h predictions

par(mfrow = c(1,2))

plot(df$X.log,xlim=c(start_plot,n+h),type="l",main="Model with best AIC",ylim=c(9,13),xlab = "t", ylab = "log(X)")

pred.arima.out.AIC <- predict(arima.out.AIC,n.ahead = h)
X.log.hpred.trend <- beta0 + beta1*((df$t[length(df$t)]+1):(df$t[length(df$t)]+h))
X.log.hpred.arima.AIC <- pred.arima.out.AIC$pred + X.log.hpred.trend

lines((n+1):(n+h),X.log.hpred.arima.AIC,col="red",lwd=2)


# prediction interval:
X.log.hpred.se.arima.AIC <- pred.arima.out.AIC$se
lines(X.log.hpred.arima.AIC + 1.96 * X.log.hpred.se.arima.AIC ~ c((n+1):(n+h)),col="blue",lty=3 )
lines(X.log.hpred.arima.AIC - 1.96 * X.log.hpred.se.arima.AIC ~ c((n+1):(n+h)),col="blue",lty=3 )

## Extra: What about bootstrapping? Sometimes it can be useful!
B <- 1000

residuals <- c(arima.out.AIC$residuals)
Ysim <- matrix(nrow=B,ncol=h)

#Simulate B possible futures
for(i in 1:B){
  Ysim[i,] <- simulate(arima.out.AIC, nsim=h) + X.log.hpred.trend
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
X.log.hpred.arima.BIC <- pred.arima.out.BIC$pred + X.log.hpred.trend

plot(df$X.log,xlim=c(start_plot,n+h),type="l",main="Model with best BIC",ylim=c(9,13),xlab = "t", ylab = "log(X)")

lines((n+1):(n+h),X.log.hpred.arima.BIC,col="red",lwd=2)

# prediction interval:
X.hpred.se.arima.BIC <- pred.arima.out.BIC$se
lines(X.log.hpred.arima.BIC + 1.96 * X.hpred.se.arima.BIC ~ c((n+1):(n+h)),col="blue",lty=3 )
lines(X.log.hpred.arima.BIC - 1.96 * X.hpred.se.arima.BIC ~ c((n+1):(n+h)),col="blue",lty=3 )


## Bootstrapping
B <- 1000

residuals <- c(arima.out.BIC$residuals)
Ysim <- matrix(nrow=B,ncol=h)

#Simulate B possible futures
for(i in 1:B){
  Ysim[i,] <- simulate(arima.out.BIC, nsim=h) + X.log.hpred.trend
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

