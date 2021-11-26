library(rugarch)
library(forecast)

## function for finding the best model, then plotting it
plotgarch <- function(data, arma.order, garch.model, i.max = 2, j.max = 2, h = 365) {
  
  criteria <- list()
  for(i in 1:i.max){
    criteria0 <- list()
    for(j in 1:j.max){
      model.spec <- ugarchspec(variance.model = list(model=garch.model,garchOrder=c(i,j)),mean.model = list(armaOrder=arma.order,include.mean=FALSE),distribution.model = "std")
      model=ugarchfit(spec=model.spec, data=data)
      criteria0[[j]] <- data.frame(aic=infocriteria(model)[1],bic=infocriteria(model)[2],i=i,j=j)
    }
    criteria[[i]] <- criteria0
  }
  
  model.criteria <- do.call("rbind",lapply(criteria,function(x){do.call("rbind",x)}))
  i.aic = model.criteria[which.min(model.criteria$aic),]$i
  j.aic = model.criteria[which.min(model.criteria$aic),]$j
  
  model.spec <- ugarchspec(variance.model = list(model=garch.model,garchOrder=c(i.aic,j.aic)),mean.model = list(armaOrder=arma.order,include.mean=FALSE),distribution.model = "std")
  model = ugarchfit(spec=model.spec, data=data)
  
  n <- length(data)
  
  f.model <- ugarchforecast(model,n.ahead=h)
  plot(data,type="l",xlim=c(0,(n+h)),ylab=as.character(garch.model))
  lines(seq(n+1,n+h),f.model@forecast$seriesFor,col="red")
  lines(seq(n+1,n+h),f.model@forecast$seriesFor-qt(0.025,9)*f.model@forecast$sigmaFor,col="blue")
  lines(seq(n+1,n+h),f.model@forecast$seriesFor+qt(0.025,9)*f.model@forecast$sigmaFor,col="blue")
  
  # return model
  model
}
par(mfrow=c(2,2))

# For data = Z
arma.order = c(0,2)
data = Z
h <- 365 # predict 1 year ahead

# GARCH
garch.model = "sGARCH"
mod.GARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# eGARCH
garch.model = "eGARCH"
mod.eGARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# iGARCH
garch.model = "iGARCH"
mod.iGARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# fiGARCH
garch.model = "fiGARCH"
mod.fiGARCH <- plotgarch(data,arma.order,garch.model,3,3,h)


### Extracting parameters
mod.GARCH
mod.eGARCH
mod.iGARCH
mod.fiGARCH



