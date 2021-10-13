# File to test that Durbin-Levinson algorithm works properly

### Simulating data
phi <- c(.8, .3, -.2)
n <- 500
B <- 1000
p = length(phi)

x0 <- rep(0,n+B)
for (i in (p+1):(n+B)){
  x0[i] <- phi %*% x0[(i-1):(i-p)] + rnorm(1)
}
x <- x0[-(1:B)]


### plotting
plot(x, type='l')


### estimating phi using sacf and DL
source('functions/sacf.R')
source('functions/DurbinLevinson.R')

gamma.hat <- sacf(x,max.lag=12)$gamma.hat


### own implementation
DL.out <- DL(x-mean(x),gamma.hat)

# Yule-Walker estimates for the parameters
phi.hat <- DL.out$Phi[p,1:p]
phi.hat
v <- DL.out$v
v[1:12]

sigma.sq.hat <- sum(gamma.hat[1:(p+1)] * c(1,phi.hat))
sigma.sq.hat



### Jorge

#gamma.0 <- gamma.hat[1]
#gamma.n <- gamma.hat[-1]
#DL.1step.out <- DL.1step(x-mean(x),gamma.0,gamma.n)

# Yule-Walker estimates for the parameters
#Phi <- DL.1step.out$Phi
#phi.hat <- Phi[1+p,p:1]
#phi.hat
#sigma.sq.hat <- gamma.0 + sum(gamma.n[1:p] * phi.hat)
#sigma.sq.hat

#v <- DL.1step.out$v
#v[1:12]