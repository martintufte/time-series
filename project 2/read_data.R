# Remember to set the working directory
# Session -> Set Working Directory -> To Source File Location

library(stats)
library(astsa)
library(rugarch)
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


### Testing stationarity ###

# Augmented Dickey-Fuller Test
adf.test(Y)
adf.test(Z)

# Ljung-Box test
Box.test(Y, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(Z, lag = 1, type = c("Ljung-Box"), fitdf = 0)

# ts
par(mfrow=c(2,1),mar=c(5,4,1,2))

plot(Y, type='l', xlab='t', ylab='Y')
plot(Z, type='l', xlab='t', ylab='Z')


### ARIMA ###
# For Z
AICs <- NULL
lowestAIC <- 10000
for (i in 0:3) {
  for (j in 0:4) {
    model <- arima(Z, order = c(i,0,j))
    AIC <- model$aic
    if (AIC < lowestAIC) {
      lowestAIC <- AIC
      bestARIMA <- model
      AICs[1 + i + 4*(j-1)] <- AIC
    }
  }
}
bestARIMA # best order is (1,0)
AICs

# Ljung-Box test on residuals of ARIMA-model
Box.test(bestARIMA$residuals, lag = 1, type = c("Ljung-Box"), fitdf = 0)
 

# For Y
AICs <- NULL
lowestAIC <- 10000
for (i in 0:7) {
  for (j in 0:7) {
    model <- arima(Y, order = c(i,0,j))
    AIC <- model$aic
    if (AIC < lowestAIC) {
      lowestAIC <- AIC
      bestARIMA <- model
      AICs[1 + i + 4*(j-1)] <- AIC
    }
  }
}
bestARIMA # best order is (5,2)
AICs        

# Ljung-Box test on residuals of ARIMA-model
Box.test(bestARIMA$residuals, lag = 1, type = c("Ljung-Box"), fitdf = 0)
<<<<<<< HEAD

=======
>>>>>>> b04240fbe5ad08cee0cf45c1679c39d928c1327d
plot(bestARIMA$residuals)
