
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
      arima.out <- arima(ts.log.diff,order=c(p.hat[i],0,q.hat[j]),include.mean = FALSE)
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
par(mfrow=c(1,2))
plot(arima.out.AIC$residuals)
qqnorm(arima.out.AIC$residuals)

plot(sacf(arima.out.AIC$residuals, max.lag=10)$rho.hat, type='o', xlab='lag',ylab='sacf')


# white noice test
library(tscourse)
WNtest.LB(arima.out.AIC$residuals, k=p.AIC+q.AIC+1, nparms = p.AIC + q.AIC)
hist(arima.out.AIC$residuals)

# make predictions

h = 365
par(mfrow = c(1,1))
plot(ts.log,xlim=c(0,n+h))

pred.arima.out.AIC <- predict(arima.out.AIC,n.ahead = h)
X.hpred.arima.AIC <- pred.arima.out.AIC$pred
lines((n+1):(n+h),X.hpred.arima.AIC,col="red")

# prediction interval:
X.hpred.se.arima.AIC <- pred.arima.out.AIC$se
lines(X.hpred.arima.AIC + 1.96 * X.hpred.se.arima.AIC ~ c((n+1):(n+h)),col="blue" )
lines(X.hpred.arima.AIC - 1.96 * X.hpred.se.arima.AIC ~ c((n+1):(n+h)),col="blue" )



# For BIC

which.BIC <- which(BIC == min(BIC),arr.ind = TRUE)
p.BIC <- which.BIC[1]
q.BIC <- which.BIC[2]

arima.out.BIC <- arima(ts.log,order = c(p.BIC,1,q.BIC))

# residuals
par(mfrow=c(2,2))
plot(arima.out.BIC$residuals)
qqnorm(arima.out.BIC$residuals)
plot(sacf(arima.out.BIC$residuals, max.lag=10)$rho.hat, type='o', xlab='lag',ylab='sacf')


# predictions
par(mfrow=c(1,1))
plot(ts.log,xlim=c(0,n+h))
pred.arima.out.BIC <- predict(arima.out.BIC,n.ahead = h)
X.hpred.arima.BIC <- pred.arima.out.BIC$pred
lines((n+1):(n+h),X.hpred.arima.BIC,col="red")

# prediction interval:
X.hpred.se.arima.BIC <- pred.arima.out.BIC$se
lines(X.hpred.arima.BIC + 1.96 * X.hpred.se.arima.BIC ~ c((n+1):(n+h)),col="blue" )
lines(X.hpred.arima.BIC - 1.96 * X.hpred.se.arima.BIC ~ c((n+1):(n+h)),col="blue" )
