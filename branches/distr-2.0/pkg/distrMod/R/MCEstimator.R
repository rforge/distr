###############################################################################
## Determine estimates by minimizing a given criterion
###############################################################################
MCEstimator <- function(x, ParamFamily, criterion, crit.name, interval, par, 
                        Infos, ...){
    if(!is.numeric(x))
      stop(gettext("'x' has to be a numeric vector"))
    
    if(is.null(dim(x)))
       samplesize <- length(x)
    else samplesize <- dim(x)[2]
    
    if(!is(ParamFamily, "ParamFamily"))
      stop(gettext("'ParamFamily' has to be of class 'ParamFamily'"))
    if(!is.function(criterion))
      stop(gettext("'criterion' has to be a function"))

    fun <- function(theta, Data, ParamFamily, criterion, ...){
        criterion(Data, modifyParam(ParamFamily)(theta), ...)
    }

    if(dimension(param(ParamFamily)) == 1){
        res <- optimize(f = fun, interval = interval, Data = x, 
                      ParamFamily = ParamFamily, criterion = criterion, ...)
        theta <- res$minimum
        names(theta) <- names(main(ParamFamily))
        crit <- res$objectiv
    }else{
        if(missing(par)) par <- main(ParamFamily)
        if(is(par,"Estimate")) par <- estimate(par)
        res <- optim(par = par, fn = fun, Data = x, ParamFamily = ParamFamily, 
                      criterion = criterion, ...)
        theta <- res$par
        names(theta) <- names(main(ParamFamily))
        crit <- res$value
        if(missing(crit.name))
          names(crit) <- as.character(match.call()$criterion)
        else
          names(crit) <- crit.name
    }

    if(missing(crit.name))
        est.name <- "Minimum criterion estimate"
    else
        est.name <- paste("Minimum", crit.name, "estimate", sep = " ")
    if(missing(Infos))
        Infos <- matrix(c(character(0),character(0)), ncol=2,
                        dimnames=list(character(0), c("method", "message")))
    else{
        Infos <- matrix(c(rep("MCEstimator", length(Infos)), Infos), ncol = 2)
        colnames(Infos) <- c("method", "message")
    }

    new("MCEstimate", name = est.name, estimate = theta, criterion = crit,
         Infos = Infos, samplesize = samplesize)
}
