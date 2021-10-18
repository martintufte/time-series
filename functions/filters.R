testdata <- c(486, 474, 434, 441, 435, 401, 414, 414, 386, 405, 411,
                389, 414, 426, 410, 441, 459, 449, 486, 510,
                   506, 549, 579, 581, 630, 666, 674, 729, 771, 785)


avgfilter <- function(xt, q, even = FALSE) 
{
    n <- length(xt)

    mt <- rep(0, n)
    mt
    for (t in 1:n) {
        for (i in  (t - q) : (t + q))
        {
#Functionality for avaraging over periodic data of even periodicity optimaly
            even_coeff <- 1
            if (even){
                if (i == t - q | i == t + q) {
                        even_coeff <- 0.5
                }
            }
            if (i < 1){
                mt[t] <- mt[t] + xt[1] * even_coeff
                }
            else if (i >= n)
            {
                mt[t] <- mt[t] + xt[n] * even_coeff
            }
            else
            {
                mt[t] <- mt[t] + xt[i] * even_coeff
            }
        }
        mt[t] <- mt[t] * 1 / (1 + 2 * q)
    }
    return(mt)
}

seasonality_estimator <- function(xt, d)
{
    n <- length(xt)
    #finding number of periods
    nd <- n / d
    even <- FALSE
    if (d %% 2 == 0)
        {
        q <- d / 2
        even <- TRUE
        }
    else
    {
        q <- (d - 1) / 2
    }
    #_i contains len(xt) elements, while _k contains d elements,
    #one for each element in one period
    di <- rep(0, n)#(nd,d))
    wk <- rep(0, d)
    sk <- rep(0, d)
    si <- rep(0, n)
    #Removing trend with q chosen to eliminate noice and trend
    #of seasonlized data
    di <- avgfilter(xt, q, even)
    for (k in 1:d)
        {
            for (j in 1:nd)
            {
                wk[k] <- wk[k] + (xt[k + j * d] - di[k + j * d]) / nd
            }
        }
    plot(di)
    #In case wk doesn't sum to 0, we remove it to find the seasonal component
    for (k in 1:d)
    {
        sk[k] <- wk[k] - sum(wk) / d
    }
    #plot(wk)
    #The seasonality component sk which is of
    #lenght 1 period is thereby copied nd times
    for (periods in 1:nd)
    {
        for (i in 1:d)
        {
            si[periods * d + i] <- sk[i]
        }
    }
    return(si)
}

st <- seasonality_estimator(testdata, 3)
mt <- avgfilter(testdata, 5)
# plot(testdata, type = "l")
plot(st, type = "l")
# plot(mt, type = "l")
# plot(testdata - mt, type = "l")
