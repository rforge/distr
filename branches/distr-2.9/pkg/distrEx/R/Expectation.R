## Helper function:
.filterEargs <- function(dots, neg=FALSE){
        if(length(dots)==0) return(NULL)
        formIntNames <- setdiff(unique(c(names(formals(distrExIntegrate)),
                            names(formals(integrate)),
                            names(formals(GLIntegrate)))),c("f","...", "distr"))

        if(neg) return(dots[! names(dots) %in% formIntNames])
        else return(dots[names(dots) %in% formIntNames])
}
.filterFunargs <- function(dots, fun, neg=FALSE){
        if(length(dots)==0) return(NULL)
        formFunNames <- names(formals(fun))

        if(neg) return(dots[! names(dots) %in% formFunNames])
        else return(dots[names(dots) %in% formFunNames])
}

.getIntbounds <- function(object, low, upp, lowTQ, uppTQ, IQR.fac, ...){
        qx <- q.l(object)
        fqxn <- names(formals(qx))
        dots <- list(...)
        dotsqx <- NULL
        if(length(dots)){
           dotsqx <- dots[names(dots) %in% fqxn]
           dotsqx[["lower.tail"]] <- NULL
           dotsqx[["p"]] <- NULL
        }
#        print(c(list(lowTQ, lower.tail = TRUE), dotsqx))
        low0 <- do.call(qx,c(list(lowTQ, lower.tail = TRUE), dotsqx))
        upp0 <- if ("lower.tail" %in% fqxn)
                   do.call(qx,c(list(uppTQ, lower.tail = FALSE), dotsqx)) else
                   do.call(qx,c(list(1-uppTQ), dotsqx))
        if("cond" %in% names(dotsqx)){
           m <- median(object,cond=dots$cond); s <- IQR(object,cond=dots$cond)
        }else{
           m <- median(object); s <- IQR(object)
        }
        low1 <- m - IQR.fac * s 
        upp1 <- m + IQR.fac * s
        low <- max(low0,low1,low) 
        upp <- min(upp0,upp1,upp)
        return(c(low=low,upp=upp)) 
}

## Integration of functions
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, Nsim = getdistrExOption("MCIterations"), ...){
        if(is(Symmetry(object),"SphericalSymmetry"))
           return(SymmCenter(Symmetry(object)))
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
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE
             ){
        mc <- match.call()
        if(is(Symmetry(object),"SphericalSymmetry"))
           return(SymmCenter(Symmetry(object)))
        integrand <- function(x, dfun){ x * dfun(x) }

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf

        Ib <- .getIntbounds(object, low, upp, lowerTruncQuantile, 
              upperTruncQuantile, IQR.fac)
        low <- Ib["low"]
        upp <- Ib["upp"]
        #print(Ib)
        if(upp<low) return(0)
        res <- distrExIntegrate(f = integrand,
                    lower = low,
                    upper = upp, 
                    rel.tol = rel.tol, 
                    distr = object, dfun = d(object), diagnostic = diagnostic)
        if(diagnostic){
           diagn <- attr(res, "diagnostic")
           diagn[["call"]] <- mc
           attr(res, "diagnostic") <- diagn
           class(attr(res, "diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
        if(is(Symmetry(object),"SphericalSymmetry"))
           return(SymmCenter(Symmetry(object)))
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
    function(object, low = NULL, upp = NULL, ..., diagnostic = FALSE){
             mc <- match.call()
             if(is(Symmetry(object),"SphericalSymmetry"))
                return(SymmCenter(Symmetry(object)))
             if(is.null(low)) low <- -Inf
             if(is.null(upp)) upp <- Inf
             if(upp<low) return(0)
             if(object@a >= 0){
                res0 <- E(object@X0, low = (low-object@b)/object@a,
                             upp = (upp-object@b)/object@a, ...,
                             diagnostic = diagnostic)
             }else{
                res0 <- E(object@X0, low = (upp-object@b)/object@a,
                             upp = (low-object@b)/object@a, ...,
                             diagnostic = diagnostic)
             }
             res1 <- object@a * res0 + object@b * (p(object)(upp)-p.l(object)(low))
             if(diagnostic){
                diagn <- attr(res0, "diagnostic")
                diagn[["call"]] <- mc
                attr(res1, "diagnostic") <- diagn
                class(attr(res1, "diagnostic"))<- "DiagnosticClass"
             }
             return(res1)
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
    function(object, low = NULL, upp = NULL, ...){
         getMethod("E", signature(object = "AffLinDistribution",
                         fun = "missing",
                         cond = "missing"))(object = object, low = low, upp = upp,
                                              ..., diagnostic = FALSE)
    })
setMethod("E", signature(object = "AffLinLatticeDistribution",
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
         getMethod("E", signature(object = "AffLinDistribution",
                         fun = "missing",
                         cond = "missing"))(object = object, low = low, upp = upp,
                                              ..., diagnostic = FALSE)
    })


setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, Nsim = getdistrExOption("MCIterations"), ...){
        if(is(Symmetry(object),"EllipticalSymmetry"))
              return(SymmCenter(Symmetry(object)))
        return(colMeans(r(object)(Nsim)))
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
        if(is(Symmetry(object),"EllipticalSymmetry"))
              return(SymmCenter(Symmetry(object)))
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
        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)
        funwD <- function(x) do.call(fun,c(list(x),dotsFun))
        if(useApply)
            return(mean(sapply(X=xsim, FUN=funwD)))
        else
            return(mean(fun(xsim)))
    })

setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE){

        mc <- match.call()

        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)
        funwD <- function(x) do.call(fun,c(list(x),dotsFun))

        if(useApply){
            integrand <- function(x){
                sapply(x, funwD) * d(object)(x)
            }
        }else{
            integrand <- function(x){
                funwD(x) * d(object)(x)
            }
        }

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf

        Ib <- .getIntbounds(object, low, upp, lowerTruncQuantile, 
              upperTruncQuantile, IQR.fac)
        low <- Ib["low"]
        upp <- Ib["upp"]
        res <- distrExIntegrate(f = integrand, lower = low,  upper = upp,
                    rel.tol = rel.tol, distr = object, ...,
                    diagnostic = diagnostic)

        if(diagnostic){
           diagn <- attr(res, "diagnostic")
           diagn[["call"]] <- mc
           attr(res, "diagnostic") <- diagn
           class(attr(res, "diagnostic"))<- "DiagnosticClass"
        }

        return(res)
    })

setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, low = NULL, upp = NULL, ...){
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        supp <- support(object)
        supp <- supp[supp>=low & supp<=upp]
        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)
        funwD <- function(x) do.call(fun,c(list(x),dotsFun))
        if(useApply){
            integrand <- function(x, dfun, fun){
                sapply(X=x, FUN=fun) * dfun(x)
            }
        }else{
            integrand <- function(x, dfun, fun){
                fun(x) * dfun(x)
            }
        }
        return(sum(integrand(x = supp, dfun = d(object), fun = funwD)))
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
        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)
        funwD <- function(x) do.call(fun,c(list(x),dotsFun))
        if(useApply)
            erg <- apply(X=x, MARGIN=1, FUN=funwD)
        else
            erg <- t(funwD(x))
        if(is.vector(erg))
            return(mean(erg))
        else{
            res <- funwD(x[1,])
            res[] <- rowMeans(erg)
            return(res)
        }
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, ...){
        supp <- support(object)
        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)
        funwD <- function(x) do.call(fun,c(list(x),dotsFun))
        if(useApply){
            integrand <- function(x, fun, dfun){ fun(x) * dfun(t(x)) }
            erg <- apply(supp, 1, integrand, fun = funwD, dfun = d(object))
        }else{
            erg <- t(funwD(supp) * d(object)(supp))
        }
        if(is.vector(erg))
            return(sum(erg))
        else{
            res <- funwD(supp[1,])
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
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE){
        mc <- match.call()
        fct <- function(x){ x * d(object)(x, cond) }
        if(useApply){
            integrand <- function(x){
                return(sapply(x, fct))
            }
        }else{
            integrand <- fct
        }

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf

        Ib <- .getIntbounds(object, low, upp, lowerTruncQuantile, 
                            upperTruncQuantile, IQR.fac, cond = cond)
        low <- Ib["low"]
        upp <- Ib["upp"]

        res <- distrExIntegrate(f=integrand,
              lower = low, upper = upp, rel.tol = rel.tol, distr = object,
              dfun = d(object), cond = cond, diagnostic = FALSE)

        if(diagnostic){
           diagn <- attr(res, "diagnostic")
           diagn[["call"]] <- mc
           attr(res, "diagnostic") <- diagn
           class(attr(res, "diagnostic"))<- "DiagnosticClass"
        }

        return(res)
    })

setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "missing",
                         cond = "numeric"),
    function(object, cond, useApply = TRUE, low = NULL, upp = NULL, ...){
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
             low = NULL, upp = NULL, Nsim = getdistrExOption("MCIterations"), ...){
        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)
        funwD <- function(x) do.call(fun,c(list(x),dotsFun))
        xsim <- r(object)(Nsim, cond)
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        xsim <- xsim[xsim >= low & xsim <= upp]
        if(withCond){
            if(useApply)
                res <- mean(sapply(xsim, funwD, cond))
            else
                res <- mean(funwD(xsim, cond))
        }else{
            if(useApply)
                res <- mean(sapply(xsim, funwD))
            else
                res <- mean(funwD(xsim))
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
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE){

        mc <- match.call()
        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)

        CondArg <- if(withCond) list(cond=cond) else NULL
        funwD <- function(x) do.call(fun,c(list(x), CondArg,dotsFun))

        if(useApply){
           integrand <- function(x) sapply(x, funwD) * d(object)(x, cond)
        }else{
           integrand <- function(x) funwD(x) * d(object)(x, cond)
        }

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf

        Ib <- .getIntbounds(object, low, upp, lowerTruncQuantile, 
              upperTruncQuantile, IQR.fac, cond = cond)
        low <- Ib["low"]
        upp <- Ib["upp"]
        
        res <- distrExIntegrate(f=integrand,
                lower = low, upper = upp, rel.tol = rel.tol, distr = object,
                fun = funwD, ..., diagnostic = diagnostic)

        if(diagnostic){
           diagn <- attr(res, "diagnostic")
           diagn[["call"]] <- mc
           attr(res, "diagnostic") <- diagn
           class(attr(res, "diagnostic"))<- "DiagnosticClass"
        }

        return(res)
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "function",
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, low = NULL, upp = NULL, ...){

        dotsFun <- .filterFunargs(list(...), fun, neg=FALSE)

        CondArg <- if(withCond) list(cond=cond) else NULL
        funwD <- function(x) do.call(fun,c(list(x), CondArg,dotsFun))

        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        supp <- support(object)(cond)
        supp <- supp[supp>=low & supp<=upp]

        if(useApply){
           integrand <- function(x) sapply(x, funwD) * d(object)(x, cond)
        }else{
           integrand <- function(x) funwD(x) * d(object)(x, cond)
        }
        return(sum(integrand(supp)))
    })


### added 29-03-06 P.R. 
# some exact expectations:
setMethod("E", signature(object = "Norm", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(is.null(low) && is.null(upp)){
        ret.v <- mean(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        if(low == -Inf){  
           if(upp == Inf) return(mean(object))
           else return(m1df(object, upper = upp, ...))
        }else{
           E1 <- m1df(object, upper = low, ...)
           E2 <- if(upp == Inf) 
                    mean(object) else m1df(object, upper = upp, ...)         
           return(E2-E1)
        }
    }
 })

setMethod("E", signature(object = "Beta", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...,
             diagnostic = FALSE){
        mc <- match.call()

        if(!is.null(low)) if(low <= 0) low <- NULL
        if(!is.null(upp)) if(upp >= 1) upp <- NULL
        if((!isTRUE(all.equal(ncp(object),0)))|| !is.null(low) || !is.null(upp)){

          res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

          if(diagnostic){
             diagn <- attr(res, "diagnostic")
             diagn[["call"]] <- mc
             attr(res, "diagnostic") <- diagn
             class(attr(res, "diagnostic"))<- "DiagnosticClass"
          }

          return(res)
        }else{
          ret.v <- shape1(object)/(shape1(object)+shape2(object))
          if(!propagate.names){names(ret.v) <- NULL}
          return(ret.v)
        }
    })
## source: https://mathworld.wolfram.com/BetaDistribution.html

setMethod("E", signature(object = "Binom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- size(object)*prob(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        if(low == -Inf){  
           if(upp == Inf) return(size(object)*prob(object))
           else return(m1df(object, upper = upp, ...))
        }else{
           E1 <- m1df(object, upper = low, ...)
           E2 <- if(upp == Inf) 
                    size(object)*prob(object) else m1df(object, upper = upp, ...)         
           return(E2-E1)
        }
    }
   })

### source: https://mathworld.wolfram.com/BinomialDistribution.html

setMethod("E", signature(object = "Cauchy",
                         fun = "missing",
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ..., diagnostic = FALSE){
    mc <- match.call()
    if(is.null(low) && is.null(upp))
        return(NA)
    else{
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        if(low == -Inf){
           if(upp == Inf) return(NA)
           else return(-Inf)
        }else{
           if(upp == Inf) return(Inf) else{
              res <- E(object, fun = function(x)1, low=low, upp= upp,
                       ..., diagnostic = diagnostic)

             if(diagnostic){
                diagn <- attr(res, "diagnostic")
                diagn[["call"]] <- mc
                attr(res, "diagnostic") <- diagn
                class(attr(res, "diagnostic"))<- "DiagnosticClass"
             }

             return(res)

           }
        }
    }
  })

### source https://mathworld.wolfram.com/CauchyDistribution.html

setMethod("E", signature(object = "Chisq", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- df(object)+ncp(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        if(low == -Inf){  
           if(upp == Inf) return(df(object)+ncp(object))
           else return(m1df(object, upper = upp, ...))
        }else{
           E1 <- m1df(object, upper = low, ...)
           E2 <- if(upp == Inf) 
                    df(object)+ncp(object) else m1df(object, upper = upp, ...)         
           return(E2-E1)
        }
    }
 })
### source https://mathworld.wolfram.com/Chi-SquaredDistribution.html

setMethod("E", signature(object = "Dirac", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(is.null(low) && is.null(upp)){
        ret.v <- location(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
     if(is.null(low)) low <- -Inf
     if(is.null(upp)) upp <- Inf
     return(location(object)*(location(object)>=low & location(object) <=upp))
    }})


setMethod("E", signature(object = "DExp", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ..., diagnostic = FALSE){
    mc <- match.call()
    if(is.null(low) && is.null(upp)) return(0) else{
          res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

          if(diagnostic){
             diagn <- attr(res, "diagnostic")
             diagn[["call"]] <- mc
             attr(res, "diagnostic") <- diagn
             class(attr(res, "diagnostic"))<- "DiagnosticClass"
          }

        return(res)
    }
  })

### source https://mathworld.wolfram.com/LaplaceDistribution.html

setMethod("E", signature(object = "Exp", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- 1/rate(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
        if(is.null(low)) low <- -Inf
        if(is.null(upp)) upp <- Inf
        if(low == -Inf){  
           if(upp == Inf) return(1/rate(object))
           else return(m1df(object, upper = upp, ...))
        }else{
           E1 <- m1df(object, upper = low, ...)
           E2 <- if(upp == Inf) 
                    1/rate(object) else m1df(object, upper = upp, ...)         
           return(E2-E1)
        }
    }
 })

 ### source https://mathworld.wolfram.com/ExponentialDistribution.html

setMethod("E", signature(object = "Fd", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...,
             diagnostic = FALSE){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp)){
        df1 <- df1(object)
        df2 <- df2(object)
        d <- ncp(object)
        ret.v <- ifelse(df2>2,df2/(df2-2)*(df1+d)/df1,Inf)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
      mc <- match.call()
      res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

      if(diagnostic){
         diagn <- attr(res, "diagnostic")
         diagn[["call"]] <- mc
         attr(res, "diagnostic") <- diagn
         class(attr(res, "diagnostic"))<- "DiagnosticClass"
      }

    return(res)
    }
 })
### source (without ncp) https://mathworld.wolfram.com/F-Distribution.html

setMethod("E", signature(object = "Gammad", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...,
             diagnostic = FALSE){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- shape(object)*scale(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
      mc <- match.call()
      res <- E(object, fun = function(x)1, low=low, upp=upp, ..., diagnostic = diagnostic)

      if(diagnostic){
         diagn <- attr(res, "diagnostic")
         diagn[["call"]] <- mc
         attr(res, "diagnostic") <- diagn
         class(attr(res, "diagnostic"))<- "DiagnosticClass"
      }

    return(res)
    }
 })

### source https://mathworld.wolfram.com/GammaDistribution.html

## replaced by quantile method in file GammaWeibullExpectation.R from distrEx 2.8.0
#  on
#
#setMethod("E", signature(object = "Gammad",
#                         fun = "function",
#                         cond = "missing"),
#    function(object, fun, low = NULL, upp = NULL,
#             rel.tol= getdistrExOption("ErelativeTolerance"),
#             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
#             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
#            IQR.fac = getdistrExOption("IQR.fac"), ...
#             ){
#
#        dots <- list(...)
#        dots.withoutUseApply <- dots
#        useApply <- TRUE
#        if(!is.null(dots$useApply)) useApply <- dots$useApply
#        dots.withoutUseApply$useApply <- NULL
#        integrand <- function(x, dfun, ...){   di <- dim(x)
#                                               y <- exp(x)
#                                               if(useApply){
#                                                    funy <- sapply(y,fun, ...)
#                                                    dim(y) <- di
#                                                    dim(funy) <- di
#                                               }else funy <- fun(y,...)
#                                        return(funy * y * dfun(y)) }
#
#        if(is.null(low)) low <- -Inf
#        if(is.null(upp)) upp <- Inf
#
#        Ib <- .getIntbounds(object, low, upp, lowerTruncQuantile,
#              upperTruncQuantile, IQR.fac)
#        low <- if(Ib["low"]<=0) -Inf else log(Ib["low"])
#        upp <- log(Ib["upp"])
#
#        return(do.call(distrExIntegrate, c(list(f = integrand,
#                    lower = low,
#                    upper = upp,
#                    rel.tol = rel.tol,
#                    distr = object, dfun = d(object)), dots.withoutUseApply)))
#
#    })


setMethod("E", signature(object = "Geom",
                         fun = "missing",
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- 1/ prob(object) -1
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
        return(E(as(object,"DiscreteDistribution"), low=low, upp=upp, ...))
    }})

### source https://mathworld.wolfram.com/GeometricDistribution.html

setMethod("E", signature(object = "Hyper", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- k(object)*m(object)/(m(object)+n(object))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
        return(E(as(object,"DiscreteDistribution"), low=low, upp=upp, ...))
    }})
### source https://mathworld.wolfram.com/HypergeometricDistribution.html

setMethod("E", signature(object = "Logis", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"),
             ..., diagnostic = FALSE){
    if(is.null(low) && is.null(upp)){
        ret.v <- location(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
      mc <- match.call()
      res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

      if(diagnostic){
         diagn <- attr(res, "diagnostic")
         diagn[["call"]] <- mc
         attr(res, "diagnostic") <- diagn
         class(attr(res, "diagnostic"))<- "DiagnosticClass"
      }

    return(res)
    }
 })
### source https://mathworld.wolfram.com/LogisticDistribution.html

setMethod("E", signature(object = "Lnorm", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...,
             diagnostic = FALSE){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- exp(meanlog(object)+sdlog(object)^2/2)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
      mc <- match.call()
      if(is.null(low) && is.null(upp)) return(0) else{
        mc <- match.call()
        res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

        if(diagnostic){
           diagn <- attr(res, "diagnostic")
           diagn[["call"]] <- mc
           attr(res, "diagnostic") <- diagn
           class(attr(res, "diagnostic"))<- "DiagnosticClass"
        }

      return(res)
      }}
 })
### source https://mathworld.wolfram.com/LogNormalDistribution.html

setMethod("E", signature(object = "Nbinom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"),
             ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- size(object)*(1-prob(object))/prob(object)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
        return(E(as(object,"DiscreteDistribution"), low=low, upp=upp, ...))
    }})
### source https://mathworld.wolfram.com/NegativeBinomialDistribution.html

setMethod("E", signature(object = "Pois", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    if(!is.null(low)) if(low <= min(support(object))) low <- NULL
    if(!is.null(upp)) if(upp >= max(support(object))) upp <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- (lambda(object))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else
        return(E(as(object,"DiscreteDistribution"), low=low, upp=upp, ...))    
    })
### source https://mathworld.wolfram.com/PoissonDistribution.html

setMethod("E", signature(object = "Td", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"),
             ..., diagnostic = FALSE){
        ## correction thanks to G.Jay Kerns
    if(is.null(low) && is.null(upp)){
        ret.v <- ifelse( df(object)>1,
                       ncp(object)*sqrt(df(object)/2)*
                         exp(lgamma((df(object)-1)/2)-lgamma(df(object)/2)),
                       NA)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
      mc <- match.call()
      res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

      if(diagnostic){
         diagn <- attr(res, "diagnostic")
         diagn[["call"]] <- mc
         attr(res, "diagnostic") <- diagn
         class(attr(res, "diagnostic")) <- "DiagnosticClass"
      }

    return(res)
    }
 })
### source https://mathworld.wolfram.com/NoncentralStudentst-Distribution.html
setMethod("E", signature(object = "Unif", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"),
             ..., diagnostic = FALSE){
    if(!is.null(low)) if(low <= Min(object)) low <- NULL
    if(!is.null(upp)) if(upp >= Max(object)) upp <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- (Max(object)+Min(object))/2
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
      mc <- match.call()
      res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

      if(diagnostic){
         diagn <- attr(res, "diagnostic")
         diagn[["call"]] <- mc
         attr(res, "diagnostic") <- diagn
         class(attr(res, "diagnostic"))<- "DiagnosticClass"
      }

    return(res)
    }
 })
### source https://mathworld.wolfram.com/UniformDistribution.html

setMethod("E", signature(object = "Weibull", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL,
             propagate.names=getdistrExOption("propagate.names.functionals"),
             ..., diagnostic = FALSE){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(is.null(low) && is.null(upp)){
        ret.v <- scale(object)*gamma(1+1/shape(object))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }else{
      mc <- match.call()
      res <- E(object, fun = function(x)1, low=low, upp=upp, ..., diagnostic = diagnostic)

      if(diagnostic){
         diagn <- attr(res, "diagnostic")
         diagn[["call"]] <- mc
         attr(res, "diagnostic") <- diagn
         class(attr(res, "diagnostic"))<- "DiagnosticClass"
      }

    return(res)
    }
 })
### source https://mathworld.wolfram.com/WeibullDistribution.html
setMethod("E", signature(object = "Arcsine", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ..., diagnostic = FALSE){
    if(!is.null(low)) if(low <= 0) low <- NULL
    if(!is.null(upp)) if(upp >= 1) upp <- NULL
    if(is.null(low) && is.null(upp)) return(0) else{
      mc <- match.call()
      res <- E(as(object,"AbscontDistribution"), low=low, upp=upp, ..., diagnostic = diagnostic)

      if(diagnostic){
         diagn <- attr(res, "diagnostic")
         diagn[["call"]] <- mc
         attr(res, "diagnostic") <- diagn
         class(attr(res, "diagnostic"))<- "DiagnosticClass"
      }

    return(res)
    }
 })


############################ Expectation for UnivarLebDecDistribution
### merged from Expectation_LebDec.R on Apr 15 2009
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE){
        mc <- match.call()
        if(is(Symmetry(object),"SphericalSymmetry"))
           return(SymmCenter(Symmetry(object)))
        I.ac <- E(acPart(object), low = low, upp = upp, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ..., diagnostic = diagnostic )
        I.dc <- E(discretePart(object), low = low, upp = upp )
        res <- as.vector(object@mixCoeff %*% c(I.ac, I.dc))
        if(diagnostic){
           diagn <- attr(I.ac, "diagnostic")
           diagn[["call"]] <- mc
           attr(res,"diagnostic") <- diagn
           class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "missing"),
    function(object, fun, useApply = TRUE, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... , diagnostic = FALSE){
        mc <- match.call()
        I.ac <- E(acPart(object), fun = fun, useApply = useApply,
                  low = low, upp = upp, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... , diagnostic = diagnostic)
        I.dc <- E(discretePart(object), fun = fun, useApply = useApply,
                  low = low, upp = upp, ... )
        res <- as.vector(object@mixCoeff %*% c(I.ac, I.dc))
        if(diagnostic){
           diagn <- attr(I.ac, "diagnostic")
           diagn[["call"]] <- mc
           attr(res,"diagnostic") <- diagn
           class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "ANY"),
    function(object, cond, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE ){
        mc <- match.call()
        I.ac <- E(acPart(object), cond = cond, low = low, upp = upp,
                  rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... , diagnostic = diagnostic)
        I.dc <- E(discretePart(object), cond = cond, low = low, upp = upp, ... )
        res <- as.vector(object@mixCoeff %*% c(I.ac, I.dc))
        if(diagnostic){
           diagn <- attr(I.ac, "diagnostic")
           diagn[["call"]] <- mc
           attr(res,"diagnostic") <- diagn
           class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "ANY"),
    function(object, fun, cond, useApply = TRUE, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE){
        mc <- match.call()
        I.ac <- E(acPart(object), fun = fun, cond = cond, useApply = useApply,
                  low = low, upp = upp, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... , diagnostic = diagnostic)
        I.dc <- E(discretePart(object), fun = fun, cond = cond, 
                  useApply = useApply, low = low, upp = upp, ... )
        res <- as.vector(object@mixCoeff %*% c(I.ac, I.dc))
        if(diagnostic){
           diagn <- attr(I.ac, "diagnostic")
           diagn[["call"]] <- mc
           attr(res,"diagnostic") <- diagn
           class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })

setMethod("E", signature(object = "AffLinUnivarLebDecDistribution",
                         fun = "missing",
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution",
                         fun = "missing",
                         cond = "missing")))

setMethod("E", signature(object = "AcDcLcDistribution",
                         fun = "ANY",
                         cond = "ANY"),
    function(object, fun, cond, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE ){
        mc <- match.call()
        object <- .ULC.cast(object)
        I.ac <- E(acPart(object), fun = fun, cond = cond, low = low, upp = upp, 
                  rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... , diagnostic = diagnostic)
        I.dc <- E(discretePart(object), fun = fun, cond = cond, low = low, 
                  upp = upp, ... )
        res <- as.vector(object@mixCoeff %*% c(I.ac, I.dc))
        if(diagnostic){
           diagn <- attr(I.ac, "diagnostic")
           diagn[["call"]] <- mc
           attr(res,"diagnostic") <- diagn
           class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })

setMethod("E", signature(object = "CompoundDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ..., diagnostic = FALSE){
        mc <- match.call()
         S <- object@SummandsDistr
         N <- object@NumbOfSummandsDistr
        if(!is.null(low)) if(low <= q.l(object)(0)) low <- NULL
        if(!is.null(upp)) if(upp >= q.l(object)(1)) upp <- NULL
 
       if(is(S,"UnivariateDistribution") && is.null(low) && is.null(upp)){
          resS <- E(S, ..., diagnostic = diagnostic)
          resN <- E(N)
          res <- resS*resN
          if(diagnostic){
             diagn <- attr(resS, "diagnostic")
             diagn[["call"]] <- mc
             attr(res,"diagnostic") <- diagn
             class(attr(res,"diagnostic"))<- "DiagnosticClass"
          }
          return(res)
       }else{
          res <- E(simplifyD(object), low = low, upp = upp, ..., diagnostic = diagnostic)
          if(diagnostic){
             diagn <- attr(res, "diagnostic")
             diagn[["call"]] <- mc
             attr(res, "diagnostic") <- diagn
             class(attr(res,"diagnostic"))<- "DiagnosticClass"
          }
          return(res)
       }
    })

setMethod("E", signature(object = "UnivarMixingDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE){
        mc <- match.call()
        l <- length(object@mixCoeff)
        Ei <- numeric(l)
        diagn <- if(diagnostic) vector("list",l) else NULL
        for(i in 1:l){
            buf <- E(object = object@mixDistr[[i]], low = low, upp = upp,
                     rel.tol = rel.tol, lowerTruncQuantile = lowerTruncQuantile,
                     upperTruncQuantile = upperTruncQuantile,
                     IQR.fac = IQR.fac, ..., diagnostic = diagnostic)
            Ei[i] <- buf * object@mixCoeff[i]
            if(diagnostic) diagn[[i]] <- attr(buf, "diagnostic")
        }
        res <- sum(Ei)
        if(diagnostic){
          diagn[["call"]] <- mc
          attr(res, "diagnostic") <- diagn
          class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })
setMethod("E", signature(object = "UnivarMixingDistribution",
                         fun = "function",
                         cond = "missing"),
    function(object, fun, useApply = TRUE, low = NULL, upp = NULL,
             rel.tol= getdistrExOption("ErelativeTolerance"),
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"),
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"),
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE ){
        mc <- match.call()
        l <- length(object@mixCoeff)
        Ei <- numeric(l)
        diagn <- if(diagnostic) vector("list",l) else NULL
        for(i in 1:l){
            buf <- E(object = object@mixDistr[[i]], fun = fun, low = low,
                     upp = upp, rel.tol = rel.tol,
                     lowerTruncQuantile = lowerTruncQuantile,
                     upperTruncQuantile = upperTruncQuantile,
                     IQR.fac = IQR.fac, ..., diagnostic = diagnostic)
            Ei[i] <- buf * object@mixCoeff[i]
            if(diagnostic) diagn[[i]] <- attr(buf, "diagnostic")
        }
        res <- sum(Ei)
        if(diagnostic){
          diagn[["call"]] <- mc
          attr(res, "diagnostic") <- diagn
          class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })
setMethod("E", signature(object = "UnivarMixingDistribution",
                         fun = "missing",
                         cond = "ANY"),
    function(object, cond, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE ){
        mc <- match.call()
        l <- length(object@mixCoeff)
        Ei <- numeric(l)
        diagn <- if(diagnostic) vector("list",l) else NULL
        for(i in 1:l){
            buf <- E(object = object@mixDistr[[i]], cond = cond, low = low,
                     upp = upp, rel.tol = rel.tol,
                     lowerTruncQuantile = lowerTruncQuantile,
                     upperTruncQuantile = upperTruncQuantile,
                     IQR.fac = IQR.fac, ..., diagnostic = diagnostic)
            Ei[i] <- buf * object@mixCoeff[i]
            if(diagnostic) diagn[[i]] <- attr(buf, "diagnostic")
        }
        res <- sum(Ei)
        if(diagnostic){
          diagn[["call"]] <- mc
          attr(res, "diagnostic") <- diagn
          class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })

setMethod("E", signature(object = "UnivarMixingDistribution",
                         fun = "function",
                         cond = "ANY"),
    function(object, fun, cond, useApply = TRUE, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ..., diagnostic = FALSE ){
        mc <- match.call()
        l <- length(object@mixCoeff)
        Ei <- numeric(l)
        diagn <- if(diagnostic) vector("list",l) else NULL
        for(i in 1:l){
            buf <- E(object = object@mixDistr[[i]], cond = cond, fun = fun,
                     low = low, upp = upp, rel.tol = rel.tol,
                     lowerTruncQuantile = lowerTruncQuantile,
                     upperTruncQuantile = upperTruncQuantile,
                     IQR.fac = IQR.fac, ..., diagnostic = diagnostic)
            Ei[i] <- buf * object@mixCoeff[i]
            if(diagnostic) diagn[[i]] <- attr(buf, "diagnostic")
        }
        res <- sum(Ei)
        if(diagnostic){
          diagn[["call"]] <- mc
          attr(res, "diagnostic") <- diagn
          class(attr(res,"diagnostic"))<- "DiagnosticClass"
        }
        return(res)
    })
