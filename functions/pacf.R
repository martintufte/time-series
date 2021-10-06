source("functions/sacf.R")


pacf <- function(x,max.lag = 12){
  n <- length(x)
  phi.hat <- numeric(max.lag+1)
  sacf <- sacf(x)
  
  for (h in 1:min(max.lag,n-1)){
    vec <- solve(toeplitz(sacf$rho.hat[1:h]), sacf$rho.hat[2:(h+1)])
    phi.hat[h] <- vec[length(vec)]
  }
  output <- list(phi.hat = phi.hat)
  return(output)
}