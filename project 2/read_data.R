# Remember to set the working directory
# Session -> Set Working Directory -> To Source File Location

library(stats)
library(astsa)



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

# ts
plot(X, type='l', xlab='t', ylab='USD')
plot(X.log, type='l', xlab='t', ylab='log(USD)')
plot(X.diff, type='l', xlab='t', ylab='diff USD')
plot(X.log.diff, type='l', xlab='t', ylab='diff log(USD)')
