library(stats)
library(tseries)
library(rugarch)

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

df['X.log'] <- log(df$X)

X <- df$X
X.log <- df$X.log
X.diff <- diff(df$X)
Z <- as.ts(diff(df$X.log))

arma.order = c(0,0)
garch.model = "eGARCH"

## eGARCH model 
criteria <- list()
for(i in 1:2){
  criteria0 <- list()
  for(j in 1:2){
    model.spec <- ugarchspec(variance.model = list(model=garch.model,garchOrder=c(i,j)),mean.model = list(armaOrder=arma.order,include.mean=FALSE),distribution.model = "std")
    model=ugarchfit(spec=model.spec, data=Z)
    criteria0[[j]] <- data.frame(aic=infocriteria(model)[1],bic=infocriteria(model)[2],i=i,j=j)
  }
  criteria[[i]] <- criteria0
}

model.criteria <- do.call("rbind",lapply(criteria,function(x){do.call("rbind",x)}))
#model.criteria[which.min(model.criteria$bic),]
#model.criteria[which.min(model.criteria$aic),]

i.aic <- model.criteria[which.min(model.criteria$aic),]$i
j.aic <- model.criteria[which.min(model.criteria$aic),]$j

model.spec=ugarchspec(variance.model=list(model = garch.model, garchOrder=c(i.aic,j.aic)), mean.model=list(armaOrder=arma.order), distribution.model = "std")
model=ugarchfit(spec=model.spec, data=Z)

#plot(model,which="all")

library(forecast)

h <- 365 # predict 1 year ahead
n <- length(Z)

f.model <- ugarchforecast(model,n.ahead=h)
plot(1:n,Z[1:n],type="l",xlim=c(0,(n+h)))
lines(seq(n+1,n+h),f.model@forecast$seriesFor,col="red")
lines(seq(n+1,n+h),f.model@forecast$seriesFor-qt(0.025,9)*f.model@forecast$sigmaFor,col="blue")
lines(seq(n+1,n+h),f.model@forecast$seriesFor+qt(0.025,9)*f.model@forecast$sigmaFor,col="blue")


