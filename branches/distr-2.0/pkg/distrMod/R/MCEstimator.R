###############################################################################
## Determine estimates by minimizing a given criterion
###############################################################################
MCEstimator <- function(x, ParamFamily, criterion, crit.name, 
                        startPar = NULL, 
                        Infos, trafo = NULL, penalty = 0, validity.check = TRUE,
                        asvar.fct, ...){
    es.call <- match.call()

    lmx <- length(main(ParamFamily))
    lnx <- length(nuisance(ParamFamily))

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
        vP <- validParameter(ParamFamily, theta)
        if(!vP) theta <- makeOKPar(ParamFamily)(theta)
        if(lnx)
           names(theta) <- c(names(main(ParamFamily)),
                             names(nuisance(ParamFamily)))
        else  names(theta) <- names(main(ParamFamily))
        crit <- criterion(Data, ParamFamily@modifyParam(theta), ...)
        critP <- crit + penalty * (1-vP)
        return(critP)
    }

    if(is.null(startPar)) startPar <- startPar(ParamFamily)(x,...)

    if(length(param(ParamFamily)) == 1){
        res <- optimize(f = fun, interval = startPar, Data = x, 
                      ParamFamily = ParamFamily, criterion = criterion, ...)
        theta <- res$minimum
        names(theta) <- names(main(ParamFamily))
        crit <- res$objectiv
    }else{
        if(is(startPar,"Estimate")) startPar <- untransformed.estimate(startPar)
        res <- optim(par = startPar, fn = fun, Data = x, ParamFamily = ParamFamily, 
                     criterion = criterion, ...)
        theta <- as.numeric(res$par)
        names(theta) <- c(names(main(ParamFamily)),names(nuisance(ParamFamily)))
        crit <- res$value
    }

    if(missing(crit.name)){
        names(crit) <- as.character(match.call()$criterion)
        est.name <- "Minimum criterion estimate"
    }else{
        names(crit) <- crit.name
        est.name <- paste("Minimum", crit.name, "estimate", sep = " ")
    }

    if(missing(Infos))
        Infos <- matrix(c(character(0),character(0)), ncol=2,
                        dimnames=list(character(0), c("method", "message")))
    else{
        Infos <- matrix(c(rep("MCEstimator", length(Infos)), Infos), ncol = 2)
        colnames(Infos) <- c("method", "message")
    }
    idx <-      if(lnx) lmx + 1:lnx else 1:(lmx+lnx)
    nuis.idx <- if(lnx) idx else NULL
    nuis <- if(lnx) theta[-idx] else NULL
    
    param <- ParamFamParameter(name = names(theta), 
                               main = theta[idx],
                               nuisance = nuis)    

    traf1 <- ParamFamily@param@trafo
    if(is.null(trafo)) 
         {if(is.matrix(traf1))  
             traf0 <- list(fct = function(x) 
                                 list(fval = traf1 %*% x, mat = traf1), 
                           mat = traf1)
          else
             traf0 <- list(fct = traf1, mat = traf1(main(param))$mat)                        
         }
    else {if(is.matrix(trafo))
             traf0 <- list(fct = function(x) 
                                 list(fval = trafo %*% x, mat = trafo), 
                           mat = trafo)
          else
             traf0 <- list(fct = trafo, mat = trafo(main(param))$mat)           
         } 

    if(validity.check){
        if(!validParameter(ParamFamily,param))
          {warning("Optimization for MCE did not give a valid result. You could try to use argument 'penalty'.")
           theta <- as.numeric(rep(NA, lnx+lmx))
           res <- new("MCEstimate", name = est.name, estimate = theta, 
                       criterion = crit, Infos = Infos, samplesize = samplesize, 
                       nuis.idx = nuis.idx, estimate.call = es.call, 
                       trafo = traf0)
           return(res)}
    }

    estimate <- theta[idx]
    
    asvar <- NULL
    if(!missing(asvar.fct))
       asvar <- asvar.fct(L2Fam = ParamFamily, param = param, ...)

    untransformed.estimate <- theta
    untransformed.asvar <- asvar

    if(!.isUnitMatrix(traf0$mat)){
       estimate <- traf0$fct(estimate)$fval
       if(!is.null(asvar)){
           asvar <- traf0$mat%*%asvar[idx,idx]%*%t(traf0$mat)
           rownames(asvar) <- colnames(asvar) <- c(names(estimate)) 
          }
    }
    
    new("MCEstimate", name = est.name, estimate = estimate, criterion = crit,
         asvar = asvar, Infos = Infos, samplesize = samplesize, 
         nuis.idx = nuis.idx, estimate.call = es.call, trafo = traf0,
         untransformed.estimate = untransformed.estimate,
         untransformed.asvar = untransformed.asvar)
}
