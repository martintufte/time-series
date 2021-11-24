library(MASS)

# File for finding optimal lambda for box-cox transformation


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



#fit linear regression model
model <- lm(df$X ~ df$t)

#find optimal lambda for Box-Cox transformation 
bc <- boxcox(df$X ~ df$t)
lambda <- bc$x[which.max(bc$y)]

lambda
