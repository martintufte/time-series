# Remember to set the working directory
# Session -> Set Working Directory -> To Source File Location


# file to read data
df <- read.csv(file = "data/CBBTCUSD.csv")

# number of observations
n <- dim(df)[1]

# plot the initial data
plot(1:n, df$CBBTCUSD, type='l')


