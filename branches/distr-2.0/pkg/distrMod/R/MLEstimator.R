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
MLEstimator <- function(x, ParamFamily, interval, par, Infos, trafo = NULL, penalty = 0, 
                        ...){

    es.call <- match.call()


    negLoglikelihood <- function(x, Distribution, ...){
### increase accuracy:
        if(Distribution@.withSim||!.inArgs("log",d(Distribution)))
           res <- -sum(log(Distribution@d(x, ...)))
        else  
           res <- -sum(Distribution@d(x, log = TRUE, ...))
        return(res)
    }

    
    lmx <- length(main(ParamFamily))
    lnx <- length(nuisance(ParamFamily))
    idx <- 1:lmx

    trafo0 <- diag(lnx+lmx)
    
    res <- MCEstimator(x = x, 
                ParamFamily = ParamFamily, criterion = negLoglikelihood,
                interval = interval, par = par, trafo = trafo0, 
                penalty = penalty, validity.check = FALSE, ...)

    if(!is.null(res@nuis.idx))
        idx <- -res@nuis.idx
    
    names(res@criterion) <- "negative log-likelihood"
    res@estimate.call <- es.call
    res@name <- "Maximum likelihood estimate"

    param <- ParamFamParameter(name = names(res@estimate), 
                               main = res@estimate[idx],
                               nuisance = res@estimate[-idx])
    
    if(missing(trafo)||is.null(trafo)) 
         {traf1 <- ParamFamily@param@trafo
          if(is.matrix(traf1))  
             res@trafo <- list(fct = function(x) 
                                     list(fval = traf1 %*% x, mat = traf1), 
                               mat = traf1)
          else
             res@trafo <- list(fct = traf1, mat = traf1(main(param))$mat)                        
         }
    else {if(is.matrix(trafo))
             res@trafo <- list(fct = function(x) 
                                     list(fval = trafo %*% x, mat = trafo), 
                               mat = trafo)
          else
             res@trafo <- list(fct = trafo, mat = trafo(main(param))$mat)           
         } 

    if(!validParameter(ParamFamily,param))
       {warning("Optimization for MLE did not give a valid result")
        res.estimate <- rep(NA, lnx+lmx)
        return(res)}

    asvar <- solve(FisherInfo(ParamFamily, param = param))
    res@asvar <- asvar

    return(res)
}
