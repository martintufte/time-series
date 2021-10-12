# Innovation algorithm
# Gets the theta values

source("functions/sacf.R")

K.hat <- function(X)
{
  n = length(X)
  gamma.hat <- sacf(X,max.lag=n)$gamma.hat
  
  K.hat <- matrix(NA,n+1,n+1)
  for(j in 1:(n+1))
  {
    for(i in 1:(n+1))
    {
      K.hat[i,j] <- c(gamma.hat)[1+abs(i-j)]
    }
  }
  return(K.hat)
}


innov.hstep <- function(X) {
  n = length(X)
  K = K.hat(X)
  v0 = K[1,1]
  v = rep(0,n+1)
  v[1] = v0
  Theta <- matrix(0,n,n)
  
  for (i in 1:n) {
    for (k in 0:i) {
      temp_sum = 0
      for (j in 0:(k-1)) {
        temp_sum = temp_sum + Theta[k,k-j]*Theta[n,n-j]*v[j]
      }
      Theta[i,i-k] = 1/v[k]*(K[i+1,k+1]-temp_sum)
    }
    temp_sum2 = 0
    for (j in 0:i-1) {
      temp_sum2 = temp_sum2 + (Theta[i,i-j])^2*v[j]
    }
    v[i+1] = K[i+1,i+1] - temp_sum2
  }
  return(Theta) # could return only Theta[n] as we use those to predict
}


# innov.hstep <- function(X,h)
# {
#   X.cent <- X - mean(X)
#   n <- length(X)
#   v <- numeric(n+h)
#   X.pred <- numeric(n+h)
#   Theta <- matrix(0,n+h,n+h)
#   K = K.hat(X)
#   v[1] = K[1,1]
#   X.pred[1] = 0
#   Theta[2,1] <- K[2,1]/v[1]
#   v[2] <- K[2,2] - Theta[2,1]^2 * v[1]
#   X.pred[2] <- Theta[2,1]*X[1]
#   for(k in 2:n) {
#     Theta[1+k,k] <- K[k+1,1]/v[1]
#     for (j in 2:(k-1)) {
#       Theta[1+k,k-j] <- (K[k+1,j+1] - sum(Theta[1+j,j:1]*Theta[1+k,k:(k-j+1)]*v[1:j]))/v[j+1]
#     }
#   }
# }


