## taken from RobExtremes (slightly modified) as of version 2.8.0

.qtlIntegrate <- function(object, fun, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = max(1e4,getdistrExOption("IQR.fac")), ...,
             .withLeftTail = FALSE, .withRightTail = FALSE
             ){

        dots <- list(...)
        dots.withoutUseApply <- dots
        useApply <- TRUE
        if(!is.null(dots$useApply)) useApply <- dots$useApply

        dots.withoutUseApply$useApply <- NULL
        dots.withoutUseApply$stop.on.error <- NULL

        integrand <- function(x, dfun, ...){   di <- dim(x)
                                               y <- q.l(object)(x)##quantile transformation
                                               if(useApply){
                                                    funy <- sapply(y,fun, ...)
                                                    dim(y) <- di
                                                    dim(funy) <- di
                                               }else funy <- fun(y,...)
                                        return(funy) }

         if(is.null(low)) low <- -Inf
         if(is.null(upp)) upp <- Inf

         Ib <- .getIntbounds(object, low, upp, lowerTruncQuantile,
               upperTruncQuantile, IQR.fac)
         low <- p(object)(Ib["low"])
         upp <- p(object)(Ib["upp"])
         if(is.nan(low)) low <- 0
         if(is.nan(upp)) upp <- 1

         intV.l <- intV.u <- 0
         low.m <- low
         upp.m <- upp

         if(.withRightTail){
            upp.m <- min(upp,0.98)
            if(upp>0.98){
               intV.u <- do.call(distrExIntegrate, c(list(f = integrand,
                    lower = 0.98,
                    upper = upp,
                    rel.tol = rel.tol, stop.on.error = FALSE,
                    distr = object, dfun = dunif), dots.withoutUseApply))
            }
         }
         if(.withLeftTail){
            low.m <- max(low,0.02)
            if(low<0.02){
               intV.l <- do.call(distrExIntegrate, c(list(f = integrand,
                    lower = low,
                    upper = 0.02,
                    rel.tol = rel.tol, stop.on.error = FALSE,
                    distr = object, dfun = dunif), dots.withoutUseApply))
            }
         }
         intV.m <- do.call(distrExIntegrate, c(list(f = integrand,
                    lower = low.m,
                    upper = upp.m,
                    rel.tol = rel.tol, stop.on.error = FALSE,
                    distr = object, dfun = dunif), dots.withoutUseApply))

         int <- intV.l+intV.m+intV.u

         return(int)

    }

setMethod("E", signature(object = "Weibull", fun = "function", cond = "missing"),
    function(object, fun, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = max(1e4,getdistrExOption("IQR.fac")), ...
             ){
    .qtlIntegrate(object = object, fun = fun, low = low, upp = upp,
             rel.tol= rel.tol, lowerTruncQuantile = lowerTruncQuantile,
             upperTruncQuantile = upperTruncQuantile,
             IQR.fac = IQR.fac, ...,
             .withLeftTail = FALSE, .withRightTail = TRUE)
    })

setMethod("E", signature(object = "Gammad", fun = "function", cond = "missing"),
    function(object, fun, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = max(1e4,getdistrExOption("IQR.fac")), ...
             ){
    .qtlIntegrate(object = object, fun = fun, low = low, upp = upp,
             rel.tol= rel.tol, lowerTruncQuantile = lowerTruncQuantile,
             upperTruncQuantile = upperTruncQuantile,
             IQR.fac = IQR.fac, ...,
             .withLeftTail = TRUE, .withRightTail = TRUE)
    })
