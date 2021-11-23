# Durbin-Levinson Algorithm

DL <- function(x, gamma, max.n=Inf){
  n <- min(length(x), max.n)
  v <- rep(0, n+1)
  Phi <- matrix(0, n, n)
  
  v[1] <- gamma[1]
  Phi[1, 1] <- gamma[2] / gamma[1]
  
  for (k in 2:n){
    v[k] = v[k-1] * (1 - Phi[k-1,k-1]^2)
    
    Phi[k, k] = (gamma[k+1] - sum(Phi[k-1, 1:(k-1)] * gamma[k:2])) / v[k]
    Phi[k, 1:(k-1)] = Phi[k-1, 1:(k-1)] - Phi[k, k] * Phi[k-1, (k-1):1]
  }
  
  output <- list(Phi = Phi, v = v, lags = 0:n)
  return(output)
}


DL.1step <- function(X, q, Phi, gamma){
  n <- length(X)
  X.bar <- mean(X)
  X.pred <- sum(Phi[q,1:q] * (X - X.bar)[n:(n-q+1)] ) + X.bar
  
  return(X.pred)
}


DL.hstep <- function(X, q, Phi, gamma, h){
  X.pred <- rep(0,h)
  
  X.pred[1] <- DL.1step(X, q, Phi, gamma)
  for(i in 2:h){
    X.pred[i] <- DL.1step(c(X,X.pred[1:i-1]), q, Phi, gamma)
  }
  
  return(X.pred)
}



'# Jorges implementation
DL.1step <- function(X, gamma.0, gamma.n){
  X.cent <- X - mean(X) # Zero-mean time series
  n <- length(X)
  alpha <- numeric(n)
  v <- numeric(n+1)
  Phi <- matrix(0, n+1, n)
  v[1] <- gamma.0
  Phi[2, 1] <- gamma.n[1] / gamma.0
  alpha[1] <- Phi[2, 1]
  for (k in 1:(n-1)){
    v[k+1] <- v[k] * (1-Phi[1+k, 1]^2) # MSE
    Phi[1+k+1, 1] <- (gamma.n[k+1] - sum(gamma.n[1:k]*Phi[1+k, 1:k])) / v[k+1]
    Phi[1+k+1, (k+1):2] <- Phi[1+k, k:1] - Phi[1+k+1, 1] * Phi[1+k, 1:k]
  }
  v[n+1] <- v[n] * (1-Phi[n+1, 1]^2)
  
  output <- list(Phi = Phi, v = v)
  return(output)
}'