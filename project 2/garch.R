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
Z.mod.GARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# eGARCH
garch.model = "eGARCH"
Z.mod.eGARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# iGARCH
garch.model = "iGARCH"
Z.mod.iGARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# fiGARCH
garch.model = "fiGARCH"
Z.mod.fiGARCH <- plotgarch(data,arma.order,garch.model,3,3,h)

# For data = Y

arma.order = c(3,2)
data = Y
h <- 365 # predict 1 year ahead

# GARCH
garch.model = "sGARCH"
Y.mod.GARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# eGARCH
garch.model = "eGARCH"
Y.mod.eGARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# iGARCH
garch.model = "iGARCH"
Y.mod.iGARCH <- plotgarch(data,arma.order,garch.model,5,5,h)

# fiGARCH
garch.model = "fiGARCH"
Y.mod.fiGARCH <- plotgarch(data,arma.order,garch.model,3,3,h)


### Extracting parameters
Z.mod.GARCH
Z.mod.eGARCH
Z.mod.iGARCH
Z.mod.fiGARCH

# transform Z into something similar to Y, then plot it
transform.data <- function(garch.model,model.name,data = Z, data2 = Y, h=365) {
  y = data2[length(data2)]
  
  f.model <- ugarchforecast(garch.model,n.ahead=h)
  
  line1 <- f.model@forecast$seriesFor
  line2 <- f.model@forecast$seriesFor-qt(0.025,9)*f.model@forecast$sigmaFor
  line3 <- f.model@forecast$seriesFor+qt(0.025,9)*f.model@forecast$sigmaFor
  
  line1 = line1 + Z.mu
  line2 = line2 + Z.mu
  line3 = line3 + Z.mu
  
  
  l = length(line1)
  for(i in 2:l) {
    line1[i] = line1[i] + line1[i-1]
    line2[i] = line2[i] + line2[i-1]
    line3[i] = line3[i] + line3[i-1]
  }
  line2 = line2/sqrt(l)
  line3 = line3/sqrt(l)
  
  line1 = line1 + y
  line2 = line2 + y
  line3 = line3 + y
  
  
  plot(data2,type="l",xlim=c(0,(n+h)),ylab=model.name,ylim=c(-3,3))
  lines(seq(n+1,n+h),line1,col="red")
  lines(seq(n+1,n+h),line2,col="blue")
  lines(seq(n+1,n+h),line3,col="blue")
  
  # return model
}

# plot forecasts for Z, transformed back to non-differentiated form
par(mfrow=c(2,2))
{
  transform.data(Z.mod.GARCH,"GARCH")
  transform.data(Z.mod.eGARCH,"eGARCH")
  transform.data(Z.mod.iGARCH,"iGARCH")
  transform.data(Z.mod.fiGARCH,"fiGARCH")
}




