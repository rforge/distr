## Integration of functions
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, Nsim = getdistrExOption("MCIterations"), ...){
        xsim <- r(object)(Nsim)
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        xsim <- xsim[xsim >= low & xsim <= upp]
        return(mean(xsim))
    })
setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ...
             ){
        integrand <- function(x, dfun){ x * dfun(x) }

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf

        low0 <- q(object)(lowerTruncQuantile, lower.tail = TRUE) 
        upp0 <- q(object)(upperTruncQuantile, lower.tail = FALSE)
        m <- median(object); s <- IQR(object)
        low1 <- m - IQR.fac * s 
        upp1 <- m + IQR.fac * s
        low <- max(low0,low1,low) 
        upp <- min(upp0,upp1,upp) 
        
        return(distrExIntegrate(f = integrand, 
                    lower = low,
                    upper = upp, 
                    rel.tol = rel.tol, 
                    distr = object, dfun = d(object)))
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        supp <- support(object)
        supp <- supp[supp>=low & supp<=upp]
        dfun <- d(object)
        return(sum(supp * dfun(supp)))
    })

setMethod("E", signature(object = "LatticeDistribution", 
                         fun = "missing", 
                         cond = "missing"),
   getMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing")))


setMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
             if(is.null(low)) low <- -Inf
             if(is.null(upp)) upp <- Inf
             if(object@a >= 0)
                object@a * E(object@X0, low = object@a*low, 
                             upp = object@a*upp, ...) + object@b
             else
                object@a * E(object@X0, low = object@a*upp, 
                             upp = object@a*low, ...) + object@b
    })

setMethod("E", signature(object = "AffLinAbscontDistribution", 
                         fun = "missing", 
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing")))    
setMethod("E", signature(object = "AffLinDiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing")))    
setMethod("E", signature(object = "AffLinLatticeDistribution", 
                         fun = "missing", 
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing")))    




setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, Nsim = getdistrExOption("MCIterations"), ...){
        return(colMeans(r(object)(Nsim)))
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
        supp <- support(object)
        integrand <- function(x, dfun){ x * dfun(t(x)) }
        erg <- apply(supp, 1, integrand, dfun = d(object))
        if(is.vector(erg))
            return(sum(erg))
        else
            return(rowSums(erg))
    })
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun,  useApply = TRUE, low = NULL, upp = NULL, 
             Nsim = getdistrExOption("MCIterations"), ...){
        xsim <- r(object)(Nsim)
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        xsim <- xsim[xsim >= low & xsim <= upp]
        if(useApply)        
            return(mean(sapply(xsim, fun, ...)))
        else
            return(mean(fun(xsim, ...)))
    })

setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ...){

        if(useApply){
            integrand <- function(x, dfun, fun, ...){ 
                sapply(x, fun, ...) * dfun(x) 
            }
        }else{
            integrand <- function(x, dfun, fun, ...){ 
                fun(x, ...) * dfun(x) 
            }
        }
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        low0 <- q(object)(lowerTruncQuantile, lower.tail = TRUE) 
        upp0 <- q(object)(upperTruncQuantile, lower.tail = FALSE)
        m <- median(object); s <- IQR(object)
        low1 <- m - IQR.fac * s 
        upp1 <- m + IQR.fac * s
        low <- max(low0,low1,low) 
        upp <- min(upp0,upp1,upp) 
        
        return(distrExIntegrate(f = integrand,
                    lower = low,
                    upper = upp, 
                    rel.tol = rel.tol, 
                    distr = object, fun = fun, dfun = d(object), ...))
    })

setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, low = NULL, upp = NULL, ...){
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        supp <- support(object)
        supp <- supp[supp>=low & supp<=upp]
        if(useApply){
            integrand <- function(x, dfun, fun, ...){
                sapply(x, fun, ...) * dfun(x)
            }
        }else{
            integrand <- function(x, dfun, fun, ...){
                fun(x, ...) * dfun(x)
            }
        }
        return(sum(integrand(x = supp, dfun = d(object), fun = fun, ...)))
    })
setMethod("E", signature(object = "LatticeDistribution", 
                         fun = "function", 
                         cond = "missing"),
   getMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "function", 
                         cond = "missing")))

setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, Nsim = getdistrExOption("MCIterations"), 
             ...){
        x <- r(object)(Nsim)
        if(useApply)
            erg <- apply(x, 1, fun, ...)
        else
            erg <- t(fun(x, ...))
        if(is.vector(erg))
            return(mean(erg))
        else{
            res <- fun(x[1,], ...)
            res[] <- rowMeans(erg)
            return(res)
        }
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, ...){
        supp <- support(object)
        if(useApply){
            integrand <- function(x, fun, dfun, ...){ fun(x, ...) * dfun(t(x)) }
            erg <- apply(supp, 1, integrand, fun = fun, dfun = d(object), ...)
        }else{
            erg <- t(fun(supp, ...) * d(object)(supp))
        }
        if(is.vector(erg))
            return(sum(erg))
        else{
            res <- fun(supp[1,], ...)
            res[] <- rowSums(erg)
            return(res)
        }
    })
## Conditional expectation of functions
setMethod("E", signature(object = "UnivariateCondDistribution", 
                         fun = "missing", 
                         cond = "numeric"),
    function(object, cond, low = NULL, upp = NULL, Nsim = getdistrExOption("MCIterations"), ...){
        xsim <- r(object)(Nsim, cond)
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        xsim <- xsim[xsim >= low & xsim <= upp]
        return(mean(xsim))
    })
setMethod("E", signature(object = "AbscontCondDistribution", 
                         fun = "missing", 
                         cond = "numeric"),
    function(object, cond, useApply = TRUE, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ...
             ){
        fct <- function(x, dfun, cond){ x * dfun(x, cond) }
        if(useApply){
            integrand <- function(x, dfun, cond){ 
                return(sapply(x, fct, dfun = dfun, cond = cond))
            }
        }else{
            integrand <- fct
        }

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        low0 <- q(object)(lowerTruncQuantile, cond = cond, lower.tail = TRUE) 
        upp0 <- q(object)(upperTruncQuantile, cond = cond, lower.tail = FALSE)
        m <- median(object, cond = cond); s <- IQR(object, cond = cond)
        low1 <- m - IQR.fac * s 
        upp1 <- m + IQR.fac * s
        low <- max(low0,low1,low) 
        upp <- min(upp0,upp1,upp) 

        return(distrExIntegrate(integrand, 
              lower = low, upper = upp, rel.tol = rel.tol, distr = object, 
              dfun = d(object), cond = cond))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "missing",
                         cond = "numeric"),
    function(object,  cond, useApply = TRUE, low = NULL, upp = NULL, ...){
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        supp <- support(object)(cond)
        supp <- supp[supp>=low & supp<=upp]

        fct <- function(x, dfun, cond){ x * dfun(x, cond) }
        if(useApply)
            return(sum(sapply(supp, fct, dfun = d(object), cond = cond)))
        else
            return(sum(fct(x = supp, dfun = d(object), cond = cond)))            
    })
setMethod("E", signature(object = "UnivariateCondDistribution",
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, 
             Nsim = getdistrExOption("MCIterations"), ...){
        xsim <- r(object)(Nsim, cond)
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        xsim <- xsim[xsim >= low & xsim <= upp]
        if(withCond){
            if(useApply)
                res <- mean(sapply(xsim, fun, cond, ...))
            else
                res <- mean(fun(xsim, cond, ...))
        }else{
            if(useApply)
                res <- mean(sapply(xsim, fun, ...))
            else
                res <- mean(fun(xsim, ...))                
        }

        return(res)
    })
setMethod("E", signature(object = "AbscontCondDistribution", 
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac")
             , ...){
        if(withCond)
            if(useApply){
                integrand <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, cond, ...) * dfun(x, cond) 
                }        
            }else{
                integrand <- function(x, dfun, fun, cond, ...){ 
                    fun(x, cond, ...) * dfun(x, cond) 
                }        
            }
        else
            if(useApply){
                integrand <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, ...) * dfun(x, cond) 
                }
            }else{
                integrand <- function(x, dfun, fun, cond, ...){ 
                    fun(x, ...) * dfun(x, cond) 
                }
            }

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        low0 <- q(object)(lowerTruncQuantile, cond = cond, lower.tail = TRUE) 
        upp0 <- q(object)(1-upperTruncQuantile, cond = cond, lower.tail = FALSE)
        m <- median(object, cond = cond); s <- IQR(object, cond = cond)
        low1 <- m - IQR.fac * s 
        upp1 <- m + IQR.fac * s
        low <- max(low0,low1,low) 
        upp <- min(upp0,upp1,upp) 
        
        return(distrExIntegrate(integrand, 
                lower = low, upper = upp, rel.tol = rel.tol, distr = object, 
                dfun = d(object), fun = fun, cond = cond, ...))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "function",
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, low = NULL, upp = NULL, ...){
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        supp <- support(object)(cond)
        supp <- supp[supp>=low & supp<=upp]
        if(withCond){
            if(useApply){
                fct <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, cond, ...) * dfun(x, cond) 
                }
            }else{
                fct <- function(x, dfun, fun, cond, ...){ 
                    fun(x, cond, ...) * dfun(x, cond) 
                }
            }
        }else{
            if(useApply){
                fct <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, ...) * dfun(x, cond) 
                }
            }else{
                fct <- function(x, dfun, fun, cond, ...){ 
                    fun(x, ...) * dfun(x, cond) 
                }
            }
        }
        return(sum(fct(x = supp, dfun = d(object), fun = fun, 
                       cond = cond, ...)))
    })


### added 29-03-06 P.R. 
# some exact expectations:
setMethod("E", signature(object = "Norm", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,...){
    if(is.null(low) && is.null(upp))
        return(mean(object))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Beta", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
        if(!is.null(low)) if(low <= 0) low <- NULL
        if(!is.null(upp)) if(upp >= 1) upp <- NULL
        if((!isTRUE(all.equal(ncp(object),0)))|| !is.null(low) || !is.null(upp))
          return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))
        else
          return(shape1(object)/(shape1(object)+shape2(object)))
    })

setMethod("E", signature(object = "Binom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp))
        return(size(object)*prob(object))
    else
        return(E(as(object,"DiscreteDistribution"), low, up, ...))    
    })

setMethod("E", signature(object = "Cauchy", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(is.null(low) && is.null(upp))
        return(NA)
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Chisq", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp))
        return(df(object)+ncp(object))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Dirac", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(is.null(low) && is.null(upp))
        return(location(object))
    else{ 
     if(is.null(low)) low <- -Inf
     if(is.null(upp)) upp <- Inf
     return(location(object)*(location(object)>=low & location(object) <=upp))
    }})


setMethod("E", signature(object = "DExp", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(is.null(low) && is.null(upp))
        return(0)
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Exp", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp))
        return(1/rate(object))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })


setMethod("E", signature(object = "Fd", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){ 
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp)){
        df1 <- df1(object)
        df2 <- df2(object)
        d <- ncp(object)
        return(ifelse(df2>2,df2/(df2-2)*(df1+d)/df1,Inf))
     }   
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Gammad", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp))
        return(shape(object)*scale(object))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Geom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp))
        return(1/ prob(object) -1)
    else
        return(E(as(object,"DiscreteDistribution"), low, up, ...))    
    })

setMethod("E", signature(object = "Hyper", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp))
        return(k(object)*m(object)/(m(object)+n(object)))
    else
        return(E(as(object,"DiscreteDistribution"), low, up, ...))    
    })

setMethod("E", signature(object = "Logis", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(is.null(low) && is.null(upp))
        return(location(object))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Lnorm", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp))
        return(exp(meanlog(object)+sdlog(object)^2/2))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Nbinom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp))
        return(size(object)*(1-prob(object))/prob(object))
    else
        return(E(as(object,"DiscreteDistribution"), low, up, ...))    
    })

setMethod("E", signature(object = "Pois", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp))
        return(lambda(object))
    else
        return(E(as(object,"DiscreteDistribution"), low, up, ...))    
    })

setMethod("E", signature(object = "Td", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
        ## correction thanks to G.Jay Kerns
    if(is.null(low) && is.null(upp))
        return(ifelse( df(object)>1, 
                       ncp(object)*sqrt(df(object)/2)*
                         exp(lgamma((df(object)-1)/2)-lgamma(df(object)/2)), 
                       NA))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Unif", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= Min(object)) low <- NULL
    if(!is.null(upp)) if(upp >= Max(object)) upp <- NULL
    if(is.null(low) && is.null(upp))
        return((Max(object)+Min(object))/2)
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Weibull", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp))
        return(scale(object)*gamma(1+1/shape(object)))
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })
setMethod("E", signature(object = "Arcsine", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(!is.null(upp)) if(upp >= 1) upp <- NULL
    if(is.null(low) && is.null(upp))
        return(0)
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })

setMethod("E", signature(object = "Pareto", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
    if(!is.null(low)) if(low <= Min(object)) low <- NULL
    a <- shape(object); b <- Min(object)
    if(is.null(low) && is.null(upp)){
        if(a<=1) return(Inf)
        else return(b*a/(a-1))
     }   
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })



setMethod("E", signature(object = "Gumbel", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){a <- loc(object); b <- scale(object)
    if(is.null(low) && is.null(upp))
           return(a- EULERMASCHERONICONSTANT * b)
    else
        return(E(as(object,"AbscontDistribution"), low=low, upp=upp, ...))    
    })
## http://mathworld.wolfram.com/GumbelDistribution.html