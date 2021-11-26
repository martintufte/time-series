### DATA PREP ###

df <- read.csv(file = "data/CBBTCUSD_new.csv")

# remove earlier dates than 2021-10- (only look at new data)
df <- df[as.Date(df$DATE) >= as.Date("2021-10-12"),]
# change name of column to X


df$CBBTCUSD
df$CBBTCUSD <- as.numeric(df$CBBTCUSD)
n <- dim(df)[1]
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




predict(Z.mod.eGARCH,n.ahead=45)
