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


library(stats)
ts <- as.ts(df$CBBTCUSD)

plot(log(ts))
