## Implementation of Maximum Likelihood estimation in i.i.d. setup

# compute likelihood
#likelihood <- function(x, Distribution, ...){
#    res <- prod(Distribution@d(x, ...))
#    names(res) <- "Likelihood"
#    return(res)
#}

# compute log-likelihood
#logLikelihood <- function(x, Distribution, ...){
#    res <- sum(log(Distribution@d(x, ...)))
#    names(res) <- "Log-Likelihood"
#    return(res)
#}

# compute negative log-likelihood
#negLoglikelihood <- function(x, Distribution, ...){
#    res <- -sum(log(Distribution@d(x, ...)))
#    names(res) <- "Negative Log-Likelihood"
#    return(res)
#}

## Maximum-Likelihood estimator
MLEstimator <- function(x, ParamFamily, interval, par, ...){
    negLoglikelihood <- function(x, Distribution, ...){
        res <- -sum(log(Distribution@d(x, ...)))
        return(res)
    }

    res <- MCEstimator(x = x, ParamFamily = ParamFamily, criterion = negLoglikelihood,
                interval = interval, par = par, ...)
    names(res$criterion) <- "negative log-likelihood"
    class(res) <- c("MCEstimator", "Estimate")

    return(res)
}
