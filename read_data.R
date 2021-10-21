# Remember to set the working directory
# Session -> Set Working Directory -> To Source File Location

source('functions/sacf.R')
source('functions/pacf.R')
source('functions/DurbinLevinson.R')
source('functions/innovations.R')
source('functions/filters.R')
library(stats)
library(tseries)



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

df$t


### PLOT DATA ###

par(mfrow=c(2,2),mar=c(5,4,1,2))

df['X.log'] <- log(df$X)

X <- df$X
X.log <- df$X.log
X.diff <- diff(df$X)
X.log.diff <- diff(df$X.log)

adf.test(X)
adf.test(X.log)
adf.test(X.diff)
adf.test(X.log.diff)
adf.test(Y)

kpss.test(X)
kpss.test(X.log)
kpss.test(X.diff)
kpss.test(X.log.diff)

# ts
plot(X, type='l', xlab='t', ylab='USD')
plot(X.log, type='l', xlab='t', ylab='log(USD)')
plot(X.diff, type='l', xlab='t', ylab='diff USD')
plot(X.log.diff, type='l', xlab='t', ylab='diff log(USD)')

# sacf
plot(sacf(X, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf')
plot(sacf(X.log, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf of log')
plot(sacf(X.diff, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf of diff')
plot(sacf(X.log.diff, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf of diff of log')

# pacf
plot(pacf(X, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf')
plot(pacf(X.log, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf of log')
plot(pacf(X.diff, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf of diff')
plot(pacf(X.log.diff, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf of diff of log')



### Remove seasonality and trend

d <- 1 # no seasonality
s <- seasonality_estimator(df$X.log, d)
df['s'] <- rep(s, ceiling(n/d))[1:n]

d <- 7 # daily
s7 <- seasonality_estimator(df$X.log, d)
df['s7'] <- rep(s7, ceiling(n/d))[1:n]

d <- 30 # monthly
s30 <- seasonality_estimator(df$X.log, d)
df['s30'] <- rep(s30, ceiling(n/d))[1:n]

d <- 91 #quarterly
s91 <- seasonality_estimator(df$X.log, d)
df['s91'] <- rep(s91, ceiling(n/d))[1:n]

par(mfrow=c(3,1),mar=c(5,4,1,2))
plot(s7, type = "l")
plot(s30, type = "l")
plot(s91, type = "l")

fit <- lm(X.log ~ 1 + t, data = df) # linear trend beta0 + beta1 * t
df['m'] <- fit$fitted.values
beta0 <- fit$coefficients[1]
beta1 <- fit$coefficients[2]

# Add the stationary time series (Y = X.log - m - s)
df['Y'] = df$X.log - df$m - df$s

### Plot the time-independent time series
par(mfrow=c(1,1))
plot(as.ts(df$Y), ylab='Y')



### Model 1: MA(q) using DL ###
par(mfrow=c(1,1))
ts <- as.ts(df$Y)
n <- length(ts)
q <- 20
h <- 365

# sample autocovariance function
gamma.hat <- sacf(ts, max.lag=q)$gamma.hat

DL.out <- DL(ts-mean(ts), gamma.hat, max.n = q)

# Yule-Walker estimates for the parameters
Phi.hat <- DL.out$Phi

# Prediction
ts.pred <- DL.hstep(ts, q, Phi.hat, gamma.hat, h)
# Plot prediction
plot(ts, type='l', xlim=c(1,n+h))
lines((n+1):(n+h), ts.pred, type='l', col="red")




### Model 2: Innovations algorithm to predict
par(mfrow=c(2,2))
start.idx = 2000 # Using a limited set of data to prevent it from taking too long

ts <- as.ts(df$Y)
ts <- ts[start.idx:length(ts)] 
n <- length(ts)

# estimate K
autocor <- sacf(ts, n+h)
gamma.hat = autocor$gamma.hat

K <- toeplitz(gamma.hat)

# predict Y using innovations
Y.pred <- innov.hstep(ts,h,K)$X.pred

ts.trend <- beta0 + beta1*c(df$t[start.idx+1]:(df$t[start.idx]+n+h))

ts.pred <- Y.pred + ts.trend

ymin = min(ts.pred,((df$Y+df$m)[(start.idx+1):(start.idx+n-1)]))
ymax = max(ts.pred,((df$Y+df$m)[(start.idx+1):(start.idx+n-1)]))

# plot 
plot((start.idx+1):(start.idx+n), (df$Y+df$m)[(start.idx+1):(start.idx+n)], type="l", xlim=c(start.idx,start.idx+n+h), ylim=c(ymin,ymax), xlab='t', ylab='Y+m')
lines((start.idx+n+1):(start.idx+n+h), ts.pred[(n+1):(n+h)], col="red")

# plot the actual time series
# exp(Y + m + s)

X.pred <- exp(ts.pred)


ymin = min(X.pred,(df$X[(start.idx+1):(start.idx+n-1)]))
ymax = max(X.pred,(df$X[(start.idx+1):(start.idx+n-1)]))

plot((start.idx+1):(start.idx+n), df$X[(start.idx+1):(start.idx+n)], type="l", xlim=c(start.idx,start.idx+n+h), ylim=c(ymin,ymax), xlab='t', ylab='exp(Y+m)')
lines((start.idx+n+1):(start.idx+n+h), X.pred[(n+1):(n+h)], col="red")


# innovations using X.log instead of Y

ts.log <- X.log[start.idx:length(X.log)] # Using a limited set of data to prevent it from taking too long
n <- length(ts.log)

# estimate K
autocor <- sacf(ts.log,n+h)
gamma.hat = autocor$gamma.hat

K <- toeplitz(gamma.hat)

ts.log.pred <- innov.hstep(ts.log-mean(ts.log),h,K)$X.pred + mean(ts.log)

ymin = min(ts.log,(ts.log.pred[(n+1):(n+h)]))
ymax = max(ts.log,(ts.log.pred[(n+1):(n+h)]))

plot((start.idx+1):(n+start.idx),ts.log,type="l",xlim=c(start.idx,start.idx+n+h),ylim=c(ymin,ymax))
lines((start.idx+n+1):(start.idx+n+h),ts.log.pred[(n+1):(n+h)],col="red")

# plot actual series

ymin = min(exp(ts.log),exp(ts.log.pred[(n+1):(n+h)]))
ymax = max(exp(ts.log),exp(ts.log.pred[(n+1):(n+h)]))

plot((start.idx+1):(n+start.idx),exp(ts.log),type="l",xlim=c(start.idx,start.idx+n+h),ylim=c(ymin,ymax))
lines((start.idx+n+1):(start.idx+n+h),exp(ts.log.pred[(n+1):(n+h)]),col="red")

# Test for weekly seasonality
'par(mfrow= c(1,1))
d <- 7
s <- seasonality_estimator(X.log, d)
plot(s, xaxt = "n", ylab='Average deviations', type='o')
axis(1, at=1:7, labels=c('Mon', 'Tue', 'Wen', 'Thu', 'Fri', 'Sat', 'Sun'))''

adf.test(ts.log)
kpss.test(ts.log)
