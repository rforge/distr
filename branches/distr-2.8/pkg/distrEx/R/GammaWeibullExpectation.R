## taken from RobExtremes (slightly modified) as of version 2.8.0

.qtlIntegrate <- function(object, fun, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = max(1e4,getdistrExOption("IQR.fac")), ...,
             .withLeftTail = FALSE, .withRightTail = FALSE, diagnostic = FALSE
             ){

        mc <- match.call()
        dots <- list(...)
        dots.withoutUseApply <- .filterEargs(dots)
        useApply <- TRUE
        if(!is.null(dots$useApply)) useApply <- dots$useApply
        dots.withoutUseApply$useApply <- NULL
        dots.withoutUseApply$stop.on.error <- NULL

        ql <- if(is.null(dots$cond)) q.l(object) else function(p) q.l(object)(p,cond)

        dotsFun <- .filterFunargs(list(...), fun)
        funwD <-function(x) do.call(fun, c(list(x),dotsFun))

        integrand <- function(x){ y <- ql(x)##quantile transformation
                                  if(useApply){
                                     funy <- sapply(y,funwD)
                                     # dim(y) <- di
                                     dim(funy) <- dim(x)
                                  }else funy <- fun(y)
                                  return(funy) }
         mc <- match.call()

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

         if(diagnostic) diagn <- list(call = mc)

         if(.withRightTail){
            upp.m <- min(upp,0.98)
            if(upp>0.98){
               intV.u <- do.call(distrExIntegrate, c(list(f = integrand,
                    lower = max(0.98,low),
                    upper = upp,
                    rel.tol = rel.tol, stop.on.error = FALSE,
                    distr = object, dfun = dunif, diagnostic = diagnostic), dots.withoutUseApply))
               if(diagnostic) diagn$rightTail <- attr(intV.u,"diagnostic")
            }
         }
         if(.withLeftTail){
            low.m <- max(low,0.02)
            if(low<0.02){
               intV.l <- do.call(distrExIntegrate, c(list(f = integrand,
                    lower = low,
                    upper = min(0.02, upp),
                    rel.tol = rel.tol, stop.on.error = FALSE,
                    distr = object, dfun = dunif), dots.withoutUseApply))
               if(diagnostic) diagn$leftTail <- attr(intV.l,"diagnostic")
            }
         }
         intV.m <- do.call(distrExIntegrate, c(list(f = integrand,
                    lower = low.m,
                    upper = upp.m,
                    rel.tol = rel.tol, stop.on.error = FALSE,
                    distr = object, dfun = dunif), dots.withoutUseApply))
         if(diagnostic) diagn$main <- attr(intV.m,"diagnostic")

         int <- intV.l+intV.m+intV.u
         if(diagnostic){
            diagn[["call"]] <- mc
            attr(int,"diagnostic") <- diagn
         }
         return(int)

    }

setMethod("E", signature(object = "Weibull", fun = "function", cond = "missing"),
    function(object, fun, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = max(1e4,getdistrExOption("IQR.fac")), ..., diagnostic = FALSE
             ){
    res <- .qtlIntegrate(object = object, fun = fun, low = low, upp = upp,
             rel.tol= rel.tol, lowerTruncQuantile = lowerTruncQuantile,
             upperTruncQuantile = upperTruncQuantile,
             IQR.fac = IQR.fac, ...,
             .withLeftTail = FALSE, .withRightTail = TRUE, diagnostic = diagnostic)
    if(diagnostic){
       diagn <- attr(res,"diagnostic")
       diagn[["call"]] <- match.call()
       attr(res,"diagnostic") <- diagn
    }
    return(res)
    })

setMethod("E", signature(object = "Gammad", fun = "function", cond = "missing"),
    function(object, fun, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = max(1e4,getdistrExOption("IQR.fac")), ..., diagnostic = FALSE
             ){

    res <- .qtlIntegrate(object = object, fun, low = low, upp = upp,
             rel.tol= rel.tol, lowerTruncQuantile = lowerTruncQuantile,
             upperTruncQuantile = upperTruncQuantile,
             IQR.fac = IQR.fac, ..., .withLeftTail = TRUE,
             .withRightTail = TRUE, diagnostic = diagnostic)
    if(diagnostic){
       diagn <- attr(res,"diagnostic")
       diagn[["call"]] <- match.call()
       attr(res,"diagnostic") <- diagn
    }
    return(res)
    })


setMethod("E", signature(object = "Cauchy", fun = "function", cond = "missing"),
    function(object, fun, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = max(1e4,getdistrExOption("IQR.fac")), ...,
             diagnostic = FALSE){
    res <- .qtlIntegrate(object = object, fun = fun, low = low, upp = upp,
             rel.tol= rel.tol, lowerTruncQuantile = lowerTruncQuantile,
             upperTruncQuantile = upperTruncQuantile,
             IQR.fac = IQR.fac, ..., diagnostic = diagnostic,
             .withLeftTail = TRUE, .withRightTail = TRUE)
    if(diagnostic){
       diagn <- attr(res,"diagnostic")
       diagn[["call"]] <- match.call()
       attr(res,"diagnostic") <- diagn
    }
    return(res)
    })
