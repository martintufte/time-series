mod.GARCH
mod.eGARCH
mod.iGARCH
mod.fiGARCH


bestARIMA


# 1-step forecast with GARCH
spec = getspec(mod.GARCH)
setfixed(spec) <- as.list(coef(mod.GARCH))

new_ts <- Z[2413:2457]
new_ts

forecast <- ugarchforecast(spec, n.ahead = 1, n.roll = 44,
                           data = new_ts, out.sample = 44)
forecast
sigma(forecast)
x <- fitted(forecast)["T+1",]

