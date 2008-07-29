###############################################################################
## Implementation of Maximum Likelihood estimation in i.i.d. setup
###############################################################################

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

## caching to speed up things:
.inArgs <- distr:::.inArgs

## Maximum-Likelihood estimator
MLEstimator <- function(x, ParamFamily, interval, par, Infos, ...){
    negLoglikelihood <- function(x, Distribution, ...){
### increase accuracy:
        if(Distribution@.withSim||!.inArgs("log",d(Distribution)))
           res <- -sum(log(Distribution@d(x, ...)))
        else  
           res <- -sum(Distribution@d(x, log = TRUE, ...))
        return(res)
    }

    res <- MCEstimator(x = x, ParamFamily = ParamFamily, criterion = negLoglikelihood,
                interval = interval, par = par, ...)
    names(res@criterion) <- "negative log-likelihood"
    res@name <- "Maximum likelihood estimate"
    res@asvar <- FisherInfo(ParamFamily, param = res@estimate)
    return(res)
}
