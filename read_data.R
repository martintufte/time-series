# Remember to set the working directory
# Session -> Set Working Directory -> To Source File Location


# file to read data
df <- read.csv(file = "data/CBBTCUSD.csv")

# convert to numeric values
df$CBBTCUSD <- as.numeric(df$CBBTCUSD)

# number of observations
n <- dim(df)[1]


# NA handling
idx.na <- which( is.na(df$CBBTCUSD) )
for (i in idx.na){
  if (i == 0){
    df$CBBTCUSD[i] = df$CBBTCUSD[i+1]
  }
  else if (i == n){
    df$CBBTCUSD[i] = df$CBBTCUSD[i-1]
  }
  else {
    df$CBBTCUSD[i] = 1/2 * (df$CBBTCUSD[i-1] + df$CBBTCUSD[i+1])
  }
}


# create time-series object
library(stats)
ts <- as.ts(df$CBBTCUSD)

# log of time series
ts.log <- log(ts)

# Import sample autocovariance function
source('functions/sacf.R')

par(mfrow=c(2,2),mar=c(5,4,1,2))

plot(ts, ylab='USD')
plot(ts.log, ylab='log(USD)')
plot(sacf(ts, max.lag=100)$rho.hat, type='o', xlab='lag',ylab='sacf')
plot(sacf(ts.log, max.lag=100)$rho.hat, type='o', xlab='lag',ylab='sacf of log')


p = 2
q = 2
fit <- arima(ts.log, order = c(p, 0, q))

