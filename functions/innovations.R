# Function that performs the innovation algoirthm

# X must be zero-mean
# h is the number of steps we want to predict
# K is the covariance matrix of X_1 ... X_{n+h}
# K can be estimated using an autocorrelation function


innov.hstep <- function(X, h, K) {
  n <- length(X)
  v <- numeric(n+h)
  X.pred <- numeric(n+h)
  Theta <- matrix(NA,n+h,n+h)
  v[1] <- K[1,1]
  X.pred[1] <- 0
  Theta[1,1] <- K[2,1]/v[1]
  v[2] <- K[2,2] - Theta[1,1]^2*v[1]
  X.pred[2] <- Theta[1,1]*X[1]
  # innovations algorithm
  for (k in 2:n) {
    Theta[k,k] <- K[k+1,1]/v[1]
    for (j in 1:(k-1)) {
      Theta[k,k-j] <- (K[k+1,j+1] - sum(Theta[j,j:1]*Theta[k,k:(k-j+1)]*v[1:j]))/v[j+1]
    }
    v[k+1] <- K[k+1,k+1] - sum(Theta[k,k:1]^2 * v[1:k])
    X.pred[k+1] <- sum( Theta[k,1:k] * (X[k:1] - X.pred[k:1]))
  }
  # forcast from n+1 to h
  for (k in (n+1):(n+h-1)) {
    Theta[k,k] <- K[k+1,1]/v[1]
    for (j in 1:(k-1)) {
      Theta[k,k-j] <- (K[k+1,j+1]- sum(Theta[j,j:1]*Theta[k,k:(k-j+1)]*v[1:j]))/v[j+1]
    }
    v[k+1] <- K[k+1,k+1] - sum(Theta[k,(k-n+1):k]^2 * v[n:1])
    X.pred[k+1] <- sum( Theta[k,(k-n+1):k] * (X[n:1] - X.pred[n:1]))
  }
  output <- list(X.pred = X.pred, v=v)
}




