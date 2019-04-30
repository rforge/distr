################################################################################
# some worked out functionals
################################################################################

################################################################################
#Var
################################################################################
setMethod("var", signature(x = "UnivariateDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, 
             ...){
        if(missing(useApply)) useApply <- TRUE
        dots <- list(...)
        low <- -Inf; upp <- Inf
        if(hasArg(low)) low <- dots$low
        if(hasArg(upp)) upp <- dots$upp
        ztr <- 0
        if(is(Symmetry(x),"SphericalSymmetry")){ 
             ztr <- SymmCenter(Symmetry(x))
             x0 <- x-ztr
        }
        
        LowIsUpp <- if(low == -Inf) 
                    low == -upp else .isEqual(ztr-low,upp-ztr)
        
        if(LowIsUpp && missing(cond)&&missing(fun)){
           if(is(Symmetry(x),"SphericalSymmetry"))
              return(2 * E(x0, fun = function(t)t^2, low =0, useApply = useApply, ...))
        }

        f2 <- function(t) {fun(t)^2}
        
        if(missing(cond))
            {
            m <- E(x, fun = fun, useApply = useApply, ...) 
            m2 <- E(x, fun = f2, useApply = useApply, ...)
            }
        else{
            m <- E(x, cond = cond, fun = fun, withCond  = withCond, 
                   useApply = useApply, ...) 
            m2 <- E(x, cond = cond, fun = f2, withCond  = withCond, useApply = 
                    useApply, ...)
            }
        return(m2-m^2)
    })

setMethod("var", signature(x = "AffLinDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, 
             ...){
        if (missing(fun) && missing(cond)){

            return( x@a^2* var(x@X0, withCond = withCond, useApply = useApply, 
                             ...))

        }else return(var( x = as(x, sub("AffLin","",class(x))), 
                    fun = fun, cond = cond, withCond = withCond, 
                    useApply = useApply, ... ))
    })

setMethod("var", signature(x = "AffLinAbscontDistribution"),
           getMethod("var", signature(x = "AffLinDistribution")))    
setMethod("var", signature(x = "AffLinDiscreteDistribution"),
           getMethod("var", signature(x = "AffLinDistribution")))    
setMethod("var", signature(x = "AffLinLatticeDistribution"),
           getMethod("var", signature(x = "AffLinDistribution")))    

setMethod("var", signature(x = "CompoundDistribution"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))
         return(var(as(x,"UnivarLebDecDistribution"),...))
    else{
       S <- x@SummandsDistr
       N <- x@NumbOfSummandsDistr
       if(is(S,"UnivariateDistribution")){
         return(E(N)*var(S, ...)+ (var(S, ...)+E(S, ...)^2)*var(N))
       }
       else  return(var(simplifyD(x),...))
    }})


################################################################################
#sd
################################################################################
setMethod("sd", signature(x = "UnivariateDistribution"), 
    function(x, fun, cond, withCond = FALSE, useApply = TRUE,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
      propagate.names0 <- propagate.names
      dots <- list(...)
      dots$propagate.names <- NULL
      if(missing(fun))
        {if(missing(cond))
           return(sqrt(do.call(var,c(list(x, useApply = useApply,
                                      propagate.names=propagate.names0),dots))))
        else
           return(sqrt(do.call(var,c(list(x, cond =cond, withCond = FALSE,
                                          useApply = useApply, dots)))))
      }else{
        if(missing(cond))
           return(sqrt(do.call(var,c(list(x, fun = fun, useApply = useApply, dots)))))
        else
           return(sqrt(do.call(var,c(list(x, fun = fun, cond =cond, withCond = FALSE,
                                          useApply = useApply, dots)))))
           }
    })

### overload "sd" method for "Norm" ...
setMethod("sd", signature(x = "Norm"), 
    function(x, fun, cond, withCond = FALSE, useApply = TRUE,
             propagate.names=getdistrExOption("propagate.names.functionals"), ...){
      if(missing(fun))
        {if(missing(cond)){
           ret.v <- sd(param(x))
           if(!propagate.names){names(ret.v) <- NULL}
           return(ret.v)
        }else
           return(sqrt(var(x, cond = cond, withCond = FALSE, useApply = useApply, 
                  ...)))}
      else
        {if(missing(cond))
           return(sqrt(var(x, fun = fun, useApply = useApply, ...)))
        else
           return(sqrt(var(x, fun = fun, cond = cond, withCond = FALSE, 
                  useApply = useApply,...)))}
    }) 
    


################################################################################
#median, mad, IQR
################################################################################
setMethod("median", signature(x = "UnivariateDistribution"),
    function(x){
        if(is(Symmetry(x),"SphericalSymmetry"))
           return(SymmCenter(Symmetry(x)))
        return(q.l(x)(1/2))
    })

setMethod("median", signature(x = "UnivariateCondDistribution"),
    function(x, cond){
        return(q.l(x)(1/2, cond = cond))
    })

setMethod("median", signature(x = "AffLinDistribution"),
    function(x) x@a * median(x@X0) + x@b) 

setMethod("median", signature(x = "AffLinAbscontDistribution"),
           getMethod("median", signature(x = "AffLinDistribution")))    
setMethod("median", signature(x = "AffLinDiscreteDistribution"),
           getMethod("median", signature(x = "AffLinDistribution")))    
setMethod("median", signature(x = "AffLinLatticeDistribution"),
           getMethod("median", signature(x = "AffLinDistribution")))    

setMethod("mad", signature(x = "UnivariateDistribution"),
    function(x){
        if(is(Symmetry(x),"SphericalSymmetry"))
           return(q.l(x)(3/4))
        m <- median(x)
        y <- abs(x-m) 
        return(q.l(y)(1/2))
    })

setMethod("mad", signature(x = "AffLinDistribution"),
    function(x) abs(x@a) * mad(x@X0)) 

setMethod("mad", signature(x = "AffLinAbscontDistribution"),
           getMethod("mad", signature(x = "AffLinDistribution")))    
setMethod("mad", signature(x = "AffLinDiscreteDistribution"),
           getMethod("mad", signature(x = "AffLinDistribution")))    
setMethod("mad", signature(x = "AffLinLatticeDistribution"),
           getMethod("mad", signature(x = "AffLinDistribution")))    

setMethod("IQR", signature(x = "UnivariateDistribution"),
    function(x){
        if(is(Symmetry(x),"SphericalSymmetry"))
           return(2*q.l(x)(3/4))
        return(q.l(x)(3/4)-q.l(x)(1/4))
    })

setMethod("IQR", signature(x = "UnivariateCondDistribution"),
    function(x, cond){
        return(q.l(x)(3/4, cond = cond)-q.l(x)(1/4, cond = cond))
    })

setMethod("IQR", signature(x = "DiscreteDistribution"),
    function(x) q.r(x)(3/4)-q.l(x)(1/4)
)

setMethod("IQR", signature(x = "AffLinDistribution"),
    function(x) abs(x@a) * IQR(x@X0)) 

setMethod("IQR", signature(x = "AffLinAbscontDistribution"),
           getMethod("IQR", signature(x = "AffLinDistribution")))    
setMethod("IQR", signature(x = "AffLinDiscreteDistribution"),
           getMethod("IQR", signature(x = "AffLinDistribution")))    
setMethod("IQR", signature(x = "AffLinLatticeDistribution"),
           getMethod("IQR", signature(x = "AffLinDistribution")))    

##standardization
make01 <- function(x){
    if (!is(x, "UnivariateDistribution"))
        stop("This function is for univariate distribution objects.")
    return((x-E(x))/sd(x))
    }


#################################################################
# some exact variances:
#################################################################
setMethod("var", signature(x = "Norm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"),...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))
       return(var(as(x,"AbscontDistribution"),...))
    else{
        ret.v <- sd(x)^2
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)}
    })

setMethod("var", signature(x = "Binom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"),...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))
        return(var(as(x,"DiscreteDistribution"),...))
    else{
        ret.v <- size(x)*prob(x)*(1-prob(x))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)}
    })
### source: https://mathworld.wolfram.com/BinomialDistribution.html


setMethod("var", signature(x = "Cauchy"),
    function(x,...){    
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))
      return(var(as(x,"AbscontDistribution"),...))
    else
        return(NA)
    })
### source https://mathworld.wolfram.com/CauchyDistribution.html

setMethod("var", signature(x = "Chisq"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"),...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))
       return(var(as(x,"AbscontDistribution"),...))
    else{
        ret.v <- 2*(df(x)+2*ncp(x))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)}
    })
### source https://mathworld.wolfram.com/Chi-SquaredDistribution.html

setMethod("var", signature(x = "Dirac"),
    function(x, ...){return(0)})


setMethod("var", signature(x = "DExp"),
    function(x, ...){    
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        return(2)
    })
### source https://mathworld.wolfram.com/LaplaceDistribution.html

setMethod("var", signature(x = "Exp"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
         return(var(as(x,"AbscontDistribution"),...))
    else{
        ret.v <- 1/rate(x)^2
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)}
    })

 ### source https://mathworld.wolfram.com/ExponentialDistribution.html

setMethod("var", signature(x = "Fd"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
         return(var(as(x,"AbscontDistribution"),...))
    else{df1 <- df1(x)
         df2 <- df2(x)
         d <- ncp(x)
         Ex2 <- (E(x))^2 
         Exx <- df2^2/(df2-2)/(df2-4)*((df1+d)^2+2*df1+4*d)/df1^2
         ret.v <- if(df2>4) Exx-Ex2 else NA
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)}
    })
### source (without ncp) https://mathworld.wolfram.com/F-Distribution.html

setMethod("var", signature(x = "Gammad"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
         return(var(as(x,"AbscontDistribution"),...))
    else{
        ret.v <- shape(x)*scale(x)^2
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)}
    })
### source https://mathworld.wolfram.com/GammaDistribution.html

setMethod("var", signature(x = "Geom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
         return(var(as(x,"DiscreteDistribution"),...))
    else{
        p <- prob(x)
        e0 <- 1/p-1
        ret.v <- e0+e0^2
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)}
    })
### source https://mathworld.wolfram.com/GeometricDistribution.html

setMethod("var", signature(x = "Hyper"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
         return(var(as(x,"DiscreteDistribution"),...))
    else{
        k <- k(x)
        m <- m(x)
        n <- n(x)
        ret.v <- k*n/(m+n)*m/(m+n)*(m+n-k)/(m+n-1)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)}
    })
### source https://mathworld.wolfram.com/HypergeometricDistribution.html

setMethod("var", signature(x = "Logis"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(var(as(x,"AbscontDistribution"),...))
    else{
         ret.v <- pi^2/3*scale(x)^2
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/LogisticDistribution.html

setMethod("var", signature(x = "Lnorm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(var(as(x,"AbscontDistribution"),...))
    else{
         ret.v <- exp(2*meanlog(x)+sdlog(x)^2)*(exp(sdlog(x)^2)-1)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/LogNormalDistribution.html

setMethod("var", signature(x = "Nbinom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
         return(var(as(x,"DiscreteDistribution"),...))
    else{
         p <- prob(x); e0 <- 1/p-1
         ret.v <- size(x)*(e0+e0^2)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/NegativeBinomialDistribution.html

setMethod("var", signature(x = "Pois"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(var(as(x,"DiscreteDistribution"),...))
    else{
         ret.v <- lambda(x)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/PoissonDistribution.html

setMethod("var", signature(x = "Td"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        {n <- df(x); d<- ncp(x)
        ## correction thanks to G.Jay Kerns ### corrected again P.R.
         ret.v <- ifelse( n>2, n/(n-2)*(1+d^2)
                           -d^2*n/2*exp(2*(lgamma((n-1)/2)-lgamma(n/2))), NA)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
       }
    })

### source https://mathworld.wolfram.com/NoncentralStudentst-Distribution.html

setMethod("var", signature(x = "Unif"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(var(as(x,"AbscontDistribution"),...))
    else{
         ret.v <- (Max(x)-Min(x))^2/12
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/UniformDistribution.html

setMethod("var", signature(x = "Weibull"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(var(as(x,"AbscontDistribution"),...))
    else{
         ret.v <- scale(x)^2*(gamma(1+2/shape(x))- (gamma(1 + 1/shape(x)))^2)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)

    }
    })
### source https://mathworld.wolfram.com/WeibullDistribution.html
    
setMethod("var", signature(x = "Beta"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if((hasArg(fun))||(hasArg(cond))||(!isTRUE(all.equal(ncp(x),0)))) 
        return(var(as(x,"AbscontDistribution"),...))
    else{
         a<-shape1(x); b<- shape2(x)
         ret.v <- a*b/(a+b)^2/(a+b+1)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }
    })
## source: https://mathworld.wolfram.com/BetaDistribution.html

setMethod("var", signature(x = "Arcsine"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return(1/2)})


#################################################################
# some exact medians
#################################################################

setMethod("median", signature(x = "Norm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- mean(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "Cauchy"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- location(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "Dirac"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- location(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "DExp"),
    function(x) 0)

setMethod("median", signature(x = "Exp"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- log(2)/rate(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "Geom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- ceiling(-log(2)/log(1-prob(x))-1)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "Logis"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- location(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "Lnorm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- exp(meanlog(x))
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "Unif"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- (Max(x)+Min(x))/2
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("median", signature(x = "Arcsine"),
    function(x) 0)


#################################################################
# some exact IQRs
#################################################################

setMethod("IQR", signature(x = "Norm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- 2*qnorm(3/4)*sd(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("IQR", signature(x = "Cauchy"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- 2*scale(x)*qcauchy(3/4)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("IQR", signature(x = "Dirac"),
    function(x) 0)

setMethod("IQR", signature(x = "DExp"),
    function(x) 2*log(2))

setMethod("IQR", signature(x = "Exp"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- (log(4)-log(4/3))/rate(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("IQR", signature(x = "Geom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- ceiling(log(1/4)/log(1-prob(x)))-
                max(floor(log(3/4)/log(1-prob(x))),0)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("IQR", signature(x = "Logis"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- 2*log(3)*scale(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("IQR", signature(x = "Unif"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- (Max(x)-Min(x))/2
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("IQR", signature(x = "Arcsine"),
    function(x) sqrt(2))

#################################################################
# some exact mads
#################################################################

setMethod("mad", signature(x = "Norm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- qnorm(3/4)*sd(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("mad", signature(x = "Cauchy"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- scale(x)*qcauchy(3/4)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("mad", signature(x = "Dirac"),
    function(x) 0)

setMethod("mad", signature(x = "DExp"),
    function(x) log(2))

setMethod("mad", signature(x = "Exp"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- log((1+sqrt(5))/2)/rate(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("mad", signature(x = "Geom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")) {
         p <- prob(x); pq <-  1-p
         m <- median(x); rho <- 1/2*pq^(-m)
         ret.v <- max(ceiling(-log(rho/2+sqrt(pq+rho^2/4))/log(pq)),0)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
         })

setMethod("mad", signature(x = "Logis"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- log(3)*scale(x)
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("mad", signature(x = "Unif"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals")){
    ret.v <- (Max(x)-Min(x))/4
    if(!propagate.names){names(ret.v) <- NULL}
    return(ret.v)
    }
    )

setMethod("mad", signature(x = "Arcsine"),
    function(x) sqrt(1/2))

