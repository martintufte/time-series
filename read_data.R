# Remember to set the working directory
# Session -> Set Working Directory -> To Source File Location

source('functions/sacf.R')
source('functions/pacf.R')
source('functions/DurbinLevinson.R')
library(stats)



  ### DATA PREP ###

df <- read.csv(file = "data/CBBTCUSD.csv")

# remove earlier dates than 2015-01-19 (as they contain lots of NA)
df <- df[as.Date(df$DATE) >= as.Date("2015-01-19"),]
# fix NAN at "2020-09-04"
df[df$DATE=="2020-09-04",'CBBTCUSD'] = 10493.77
# convert to numeric values
df$CBBTCUSD <- as.numeric(df$CBBTCUSD)
# number of observations
n <- dim(df)[1]



  ### PLOT DATA ###

par(mfrow=c(2,2),mar=c(5,4,1,2))

ts <- as.ts(df$CBBTCUSD)
ts.log <- log(ts)
ts.diff <- diff(ts)
ts.log.diff <- diff(ts.log)


# ts
plot(ts, ylab='USD')
plot(ts.log, ylab='log(USD)')
plot(ts.diff, ylab='differentiated USD')
plot(ts.log.diff, ylab='differentiated log(USD)')

# sacf
plot(sacf(ts, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf')
plot(sacf(ts.log, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf of log')
plot(sacf(ts.diff, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf of diff')
plot(sacf(ts.log.diff, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf of diff of log')

# pacf
plot(pacf(ts, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf')
plot(pacf(ts.log, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf of log')
plot(pacf(ts.diff, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf of diff')
plot(pacf(ts.log.diff, max.lag=30)$phi.hat, type='o', xlab='lag',ylab='pacf of diff of log')


### Model 1: MA(1) using DL ###
par(mfrow=c(1,1))

X <- ts.log.diff
gamma.hat <- sacf(X,max.lag=20)$gamma.hat

DL.out <- DL(X-mean(X), gamma.hat, max.n = 20)

# Yule-Walker estimates for the parameters
Phi.hat <- DL.out$Phi
Phi.hat[1,1]

plot(Phi.hat[20,],type='o')

v <- DL.out$v
v[1:1]






### estimate ARIMA(p,d,q) ###



### Forecasting ###

