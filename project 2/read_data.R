# Remember to set the working directory
# Session -> Set Working Directory -> To Source File Location

library(stats)
library(astsa)
#install.packages('rugarch')
library(rugarch)


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

# remove trend
fit <- lm(log(X) ~ 1 + t, data = df) # linear trend beta0 + beta1 * t
df['m'] <- fit$fitted.values
beta0 <- fit$coefficients[1]
beta1 <- fit$coefficients[2]

# Residual time series
Y <- as.ts(log(df$X) - df$m)
Z <-  as.ts(diff(log(df$X)))

# Remove mean from Z to make it stationary
Z.mu <- mean(Z)
Z <- Z - Z.mu

# ts
par(mfrow=c(2,1),mar=c(5,4,1,2))

plot(Y, type='l', xlab='t', ylab='Y')
plot(Z, type='l', xlab='t', ylab='Z')



### SARIMA ###
AICs <- NULL
lowestAIC <- 10000
for (i in 0:4) {
  for (j in 0:4) {
    model <- arima(Y, order = c(i,0,j))
    AIC <- model$aic
    if (AIC < lowestAIC) {
      lowestAIC <- AIC
      bestARIMA <- model
      AICs[1 + i + 4*(j-1)] <- AIC
    }
  }
}
bestARIMA
AICs
