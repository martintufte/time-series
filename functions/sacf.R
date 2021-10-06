# Sample autocorrelation function (Definition 1.4.4)

sacf <- function(x, max.lag=12){
  n <- length(x)
  x.bar <- mean(x)
  gamma.hat <- numeric(max.lag+1)
  
  for (h in 0:min(max.lag, n-1)){
    gamma.hat[h+1] <- 0
    for (t in 1:(n-h)){
      gamma.hat[h+1] <- gamma.hat[h+1] + (x[t] - x.bar)*(x[t+h] - x.bar)
    }
  }
  
  gamma.hat <- gamma.hat / n
  rho.hat <- gamma.hat / gamma.hat[1]
  
  output <- list( gamma.hat = gamma.hat,
                  rho.hat = rho.hat,
                  lags = 0:max.lag)
  return(output)
}
