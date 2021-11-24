#install.packages(("fpp2"))
library(stats)
library(fpp2)

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
ts <- as.ts(log(df$CBBTCUSD))
plot(ts)

p = 2
q = 2

fit <- arima(ts.log, order = c(p, 0, q))
summary(fit)
fit$coef

checkresiduals(fit)

autoplot(forecast(fit))
autoplot(fit)


