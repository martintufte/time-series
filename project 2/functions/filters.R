# average filter and seasonality estimator

avgfilter <- function(x, d){
    if (d==1) return (x)
    # assume d > 1
    q <- floor(d/2)
    n <- length(x)
    # weigted sum
    w.len = 2*q+1
    w = rep(1, w.len)
    # give half weight to endpoints if d is even
    if (d%%2 == 0){
        w[1] = 0.5
        w[w.len] = 0.5
    }
    x.extended <- c(rep(x[1],q), x, rep(x[n],q))
    for (t in 1:n){
        x[t] = 1/d * sum(x.extended[t:(t+w.len-1)] * w)
    }
    return (x)
}

seasonality_estimator <- function(x, d){
    n <- length(x)
    dev <- x - avgfilter(x, d)
    avg.dev <- rep(0, d)
    for (i in 1:d){
        avg.dev[i] = mean(dev[seq(i,n,d)])
    }
    seasonality <- avg.dev - sum(avg.dev)/d
    return (seasonality)
}

'
testdata <- c(486, 474, 434, 441, 435, 401, 414, 414, 386, 405, 411,
              389, 414, 426, 410, 441, 459, 449, 486, 510,
              506, 549, 579, 581, 630, 666, 674, 729, 771, 785)


st <- seasonality_estimator(testdata, 3)
mt <- avgfilter(testdata, 5)
# plot(testdata, type = "l")
plot(st, type = "l")
# plot(mt, type = "l")
# plot(testdata - mt, type = "l")
'