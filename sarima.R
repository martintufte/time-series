source('functions/sacf.R')

### DATA PREP ###

df <- read.csv(file = "data/CBBTCUSD.csv")

# remove earlier dates than 2015-01-19 (as they contain lots of NA)
df <- df[as.Date(df$DATE) >= as.Date("2015-01-19"),]
# fix NAN at "2020-09-04"
df[df$DATE=="2020-09-04",'CBBTCUSD'] = 10493.77
# convert to numeric values
df$CBBTCUSD <- as.numeric(df$CBBTCUSD)
# number of observations

ts <- as.ts(df$CBBTCUSD)
ts.log <- log(ts)
ts.diff <- diff(ts)
ts.log.diff <- diff(ts.log)

n <- length(ts)
s <- 7 # one week


p.hat <- c(0:2)
q.hat <- c(0:2)
P.hat <- c(0:2)
Q.hat <- c(0:2)
d <- 1
D <- 1

AIC <- BIC <- array(NA,dim=c(length(p.hat),length(q.hat),length(P.hat),length(Q.hat)))
for( j in 1:length(p.hat)) {
  for( k in 1:length(q.hat)) {
    for( l in 1:length(P.hat)) {
      for( m in 1:length(Q.hat))
      {
        # try() to avoid convergence issues
        try({
          arima.out.jklm <- arima(ts.log,
                                  order=c(p.hat[j],d,q.hat[k]),
                                  seasonal=list(order=c(P.hat[l],D,Q.hat[m]),period=s))
          AIC[j,k,l,m] <- AIC(arima.out.jklm)
          BIC[j,k,l,m] <- BIC(arima.out.jklm)
        })
      }
    }
  }
}

# Look at AIC-selected model
ind.AIC <- which(AIC == min(AIC,na.rm=TRUE), arr.ind=TRUE)
p.AIC <- p.hat[ind.AIC[1]]
q.AIC <- q.hat[ind.AIC[2]]
P.AIC <- P.hat[ind.AIC[3]]
Q.AIC <- Q.hat[ind.AIC[4]]

print(paste(p.AIC,q.AIC,P.AIC,Q.AIC)) # printing out which model we use

arima.AIC <- arima(ts.log,
                   order=c(p.AIC,d,q.AIC),
                   seasonal=list(order=c(P.AIC,D,Q.AIC),period=s))
arima.AIC

# check residuals
par(mfrow=c(2,2))
plot(sacf(arima.AIC$resid, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf')
lines(0:length(arima.BIC$resid),rep(0,length(arima.BIC$resid)+1))
qqnorm(arima.AIC$resid)
qqline(arima.AIC$resid)
nparms.AIC <- sum(p.AIC,q.AIC,P.AIC,Q.AIC,1)

# Ljung-Box test
Box.test(arima.AIC$resid,type="Ljung-Box", fitdf = nparms.AIC, lag = nparms.AIC + 1)

# look at BIC-selected model
ind.BIC <- which(BIC == min(BIC,na.rm=TRUE), arr.ind=TRUE)
p.BIC <- p.hat[ind.BIC[1]]
q.BIC <- q.hat[ind.BIC[2]]
P.BIC <- P.hat[ind.BIC[3]]
Q.BIC <- Q.hat[ind.BIC[4]]
arima.BIC <- arima(ts.log,
                   order=c(p.BIC,d,q.BIC),
                   seasonal=list(order=c(P.BIC,D,Q.BIC),period=s))

print(paste(p.BIC,q.BIC,P.BIC,Q.BIC)) # printing which model we use wrt. best BIC

arima.BIC

# check residuals
plot(sacf(arima.BIC$resid, max.lag=30)$rho.hat, type='o', xlab='lag',ylab='sacf')
lines(0:length(arima.BIC$resid),rep(0,length(arima.BIC$resid)+1))
nparms.BIC <- sum(p.BIC,q.BIC,P.BIC,Q.BIC,1)
Box.test(arima.BIC$resid,type="Ljung-Box", fitdf = nparms.BIC, lag = nparms.BIC + 1)
qqnorm(arima.BIC$resid)
qqline(arima.BIC$resid)
shapiro.test(arima.BIC$resid)


# predictions from BIC
par(mfrow = c(1,1))
n <- length(ts.log)
h <- 365
predict.arima.BIC <- predict(arima.BIC,n.ahead=h)
pred <- predict.arima.BIC$pred
upper <- predict.arima.BIC$pred + 1.96 * predict.arima.BIC$se
lower <- predict.arima.BIC$pred - 1.96 * predict.arima.BIC$se
plot(ts.log,
     xlim=c(1,(n+h)),
     type="l",
     ylim=range(ts.log,upper,lower))

lines(predict.arima.BIC$pred~c((n+1):(n+h)),col="red")
upper <- predict.arima.BIC$pred + 1.96 * predict.arima.BIC$se
lower <- predict.arima.BIC$pred - 1.96 * predict.arima.BIC$se
lines(lower~c((n+1):(n+h)),col="blue",lty=3)
lines(upper~c((n+1):(n+h)),col="blue",lty=3)

# Bootstrapping

B <- 1000

Ysim <- matrix(nrow=B,ncol=h)

for(i in 1:B){
  Ysim[i,] <- simulate(arima.BIC, nsim=h)
}

apply(Ysim,2,sd)
boot.forecast <- apply(Ysim,2,quantile,c(0.025,0.975))
# plot(as.numeric(ts.log),
#      xlim=c(1,(n+h)),
#      type="l")
lines(predict.arima.BIC$pred~c((n+1):(n+h)),col="red")
lines(boot.forecast[1,] ~ c((n+1):(n+h)),col="blue")
lines(boot.forecast[2,] ~ c((n+1):(n+h)),col="blue")

for(i in 1:10){
  lines(Ysim[i,] ~ c((n+1):(n+h)),col=i,lty=sample(c(1,2,3)))
}


