###############################################################################
## Implementation of minimum distance estimation
###############################################################################
MDEstimator <- function(x, ParamFamily, distance = KolmogorovDist, dist.name, 
                        interval, par, Infos, trafo = NULL, penalty = 0, 
                        asvar.fct, ...){
    
    es.call <- match.call()

    res <- MCEstimator(x = x, ParamFamily = ParamFamily, criterion = distance,
                interval = interval, par = par, Infos = Infos, trafo = trafo, 
                penalty = penalty, ...)

    l.e <- length(res@estimate)
    idx <- 1:l.e
    
    if(!is.null(res@nuis.idx))
        idx <- -res@nuis.idx
    
    res@estimate.call <- es.call

    if(missing(dist.name))
      dist.name <- names(distance(x, ParamFamily@distribution))
    names(res@criterion) <- dist.name
    res@name <- paste("Minimum", dist.name, "estimate", sep = " ")

        
    param <- ParamFamParameter(name = names(res@estimate), main = res@estimate[idx],
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
       {warning(gettextf("Optimization for %s did not give a valid result",
                         res@name))
        res.estimate <- rep(NA, l.e)
        return(res)}

    if(!missing(asvar.fct))
      {asvar <- asvar.fct(L2Fam = ParamFamily, param = param, ...)
       res@asvar <- asvar
       res@untransformed.asvar <- asvar
       if(!.isUnitMatrix(res@trafo$mat)){
            asvar <- res@trafo$mat%*%asvar[idx,idx]%*%t(res@trafo$mat)
            rownames(asvar) <- colnames(asvar) <- c(names(estimate))
            res@asvar <- asvar
            }
       }

    return(res)
}

