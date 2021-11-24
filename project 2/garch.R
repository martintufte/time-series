library(rugarch)
library(forecast)

## function for finding the best model, then plotting it
plotgarch <- function(arma.order,garch.model,i.max = 4, j.max = 4) {
  
  criteria <- list()
  for(i in 1:i.max){
    criteria0 <- list()
    for(j in 1:j.max){
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
  
  h <- 365 # predict 1 year ahead
  n <- length(Z)
  
  f.model <- ugarchforecast(model,n.ahead=h)
  plot(1:n,Z[1:n],type="l",xlim=c(0,(n+h)))
  lines(seq(n+1,n+h),f.model@forecast$seriesFor,col="red")
  lines(seq(n+1,n+h),f.model@forecast$seriesFor-qt(0.025,9)*f.model@forecast$sigmaFor,col="blue")
  lines(seq(n+1,n+h),f.model@forecast$seriesFor+qt(0.025,9)*f.model@forecast$sigmaFor,col="blue")
  
}

arma.order = c(0,0)
par(mfrow=c(2,2))

# GARCH
garch.model = "sGARCH"
plotgarch(arma.order,garch.model,2,2)

# eGARCH
garch.model = "eGARCH"
plotgarch(arma.order,garch.model,2,2)

#
garch.model = "iGARCH"
plotgarch(arma.order,garch.model,2,2)

#
garch.model = "fiGARCH"
plotgarch(arma.order,garch.model,2,2)


