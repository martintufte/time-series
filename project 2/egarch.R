
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
X.log.diff <- diff(df$X.log)

## eGARCH model 
criteria <- list()
for(i in 1:2){
  criteria0 <- list()
  for(j in 1:2){
    model.egarch.spec <- ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(i,j)),mean.model = list(armaOrder=c(2,4),include.mean=FALSE),distribution.model = "std")
    model.egarch=ugarchfit(spec=model.egarch.spec, data=X.log.diff)
    criteria0[[j]] <- data.frame(aic=infocriteria(model.egarch)[1],bic=infocriteria(model.egarch)[2],i=i,j=j)
  }
  criteria[[i]] <- criteria0
}

model.egarch.criteria <- do.call("rbind",lapply(criteria,function(x){do.call("rbind",x)}))
model.egarch.criteria[which.min(model.egarch.criteria$bic),]
model.egarch.criteria[which.min(model.egarch.criteria$aic),]

i.aic <- model.egarch.criteria[which.min(model.egarch.criteria$aic),]$i
j.aic <- model.egarch.criteria[which.min(model.egarch.criteria$aic),]$j


model.egarch.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(i.aic,j.aic)), mean.model=list(armaOrder=c(2,4)), distribution.model = "std")
model.egarch=ugarchfit(spec=model.egarch.spec, data=X.log.diff)

#plot(model.egarch,which="all")


library(forecast)

h <- 365 # predict 1 year ahead
n <- length(ts)


f.model.egarch <- ugarchforecast(model.egarch,n.ahead=h)
plot(1:n,X.log.diff[1:n],type="l",xlim=c(0,(n+h)))
lines(seq(n+1,n+h),f.model.egarch@forecast$seriesFor,col="red")
lines(seq(n+1,n+h),f.model.egarch@forecast$seriesFor-qt(0.025,9)*f.model.egarch@forecast$sigmaFor,col="blue")
lines(seq(n+1,n+h),f.model.egarch@forecast$seriesFor+qt(0.025,9)*f.model.egarch@forecast$sigmaFor,col="blue")
