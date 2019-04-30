
###################################################################################
#skewness --- code due to G. Jay Kerns, gkerns@ysu.edu
###################################################################################
setMethod("skewness", signature(x = "UnivariateDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, ...){
        if(missing(cond)&&missing(fun)){
           if(is(Symmetry(x),"SphericalSymmetry"))
              return(0)
        }
        f2 <- function(t) {fun(t)^2}
        f3 <- function(t) {fun(t)^3}
        if(missing(cond))
            {
            m <- E(x, fun = fun, useApply = useApply, ...)
            m2 <- E(x, fun = f2, useApply = useApply, ...)
            m3 <- E(x, fun = f3, useApply = useApply, ...)

            return( (m3-3*m2*m+2*m^3)/(var(x, fun = fun, useApply = TRUE, ...))^1.5 )
            }
        else{
            m <- E(x, cond = cond, fun = fun, withCond  = withCond, useApply = useApply, ...)
            m2 <- E(x, cond = cond, fun = f2, withCond  = withCond, useApply = useApply, ...)
            m3 <- E(x, cond = cond, fun = f3, withCond  = withCond, useApply = useApply, ...)

            return( (m3-3*m2*m+2*m^3)/(var(x, fun = fun, cond = cond, withCond = FALSE, useApply = TRUE,...))^1.5  )

            }

    })


setMethod("skewness", signature(x = "AffLinDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, ...){
        if (missing(fun) && missing(cond)){

            return(sign(x@a)*skewness(x@X0, withCond = withCond, useApply = useApply, 
                             ...))

            }
        else return(skewness(x = as(x, sub("AffLin","",class(x))), 
                    fun = fun, cond = cond, withCond = withCond, 
                    useApply = useApply, ... ))
    })

setMethod("skewness", signature(x = "AffLinAbscontDistribution"),
           getMethod("skewness", signature(x = "AffLinDistribution")))    
setMethod("skewness", signature(x = "AffLinDiscreteDistribution"),
           getMethod("skewness", signature(x = "AffLinDistribution")))    
setMethod("skewness", signature(x = "AffLinLatticeDistribution"),
           getMethod("skewness", signature(x = "AffLinDistribution")))    
###
# some exact skewnesses:
###
#
setMethod("skewness", signature(x = "Norm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
       return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
#
setMethod("skewness", signature(x = "Binom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"),  ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
       return(skewness(as(x,"DiscreteDistribution"),...))
    else{
          ret.v <- (1-2*prob(x))/sqrt(size(x)*prob(x)*(1-prob(x)))
          if(!propagate.names){names(ret.v) <- NULL}
          return(ret.v)
    }
    })
### source: https://mathworld.wolfram.com/BinomialDistribution.html

#
setMethod("skewness", signature(x = "Cauchy"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
      return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(NA)
    })
### source https://mathworld.wolfram.com/CauchyDistribution.html

#
setMethod("skewness", signature(x = "Chisq"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
       return(skewness(as(x,"AbscontDistribution"),...))
    else{
          ret.v <- sqrt(8)*(df(x)+3*ncp(x))/(df(x)+2*ncp(x))^1.5
          if(!propagate.names){names(ret.v) <- NULL}
          return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/Chi-SquaredDistribution.html

#
setMethod("skewness", signature(x = "Dirac"),
    function(x, ...){return(NA)})

#
setMethod("skewness", signature(x = "DExp"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
         return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
### source https://mathworld.wolfram.com/LaplaceDistribution.html

#
setMethod("skewness", signature(x = "Exp"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
         return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(2)
    })
 ### source https://mathworld.wolfram.com/ExponentialDistribution.html

#
setMethod("skewness", signature(x = "Fd"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) {
         return(skewness(as(x,"AbscontDistribution"),...))
    }else {
        if (df2(x)>6){
          m <- df1(x)
          n <- df2(x)
          d <- ncp(x)
          #L <- d/m
          #m2 <- 2*n^2*(m+n-2)/m/(n-2)^2/(n-4)*(1+2*L+m*L^2/(m+n-2))
          m2 <- var(x)
          m1 <- E(x)
          m3 <- (n/m)^3/(n-2)/(n-4)/(n-6)*
                  (m^3+6*m^2+8*m+3*d*(m^2+6*m+8)+3*d^2*(m+4)+d^3)
#          a <-  8*n^3*(m+n-2)*(2*m+n-2)/m^2/(n-2)^3/(n-4)/(n-6)
#          b <-  1+3*L+6*m*L^2/(2*m+n-2)+2*m^2*L^3/(m+n-2)/(2*m+n-2)
          ret.v <- (m3-3*m2*m1-m1^3)/m2^1.5
          if(!propagate.names){names(ret.v) <- NULL}
          return(ret.v)
        } else {
          return(NA)
        }
    }
    })
### source (without ncp) https://mathworld.wolfram.com/F-Distribution.html
#
setMethod("skewness", signature(x = "Gammad"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
         return(skewness(as(x,"AbscontDistribution"),...))
    else{
        ret.v <- 2/sqrt(shape(x))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }})
### source https://mathworld.wolfram.com/GammaDistribution.html
#
setMethod("skewness", signature(x = "Geom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
         return(skewness(as(x,"DiscreteDistribution"),...))
    else{
        p <- prob(x)
        ret.v <- (2-p)/sqrt(1-p)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/GeometricDistribution.html
#
setMethod("skewness", signature(x = "Hyper"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
         return(skewness(as(x,"DiscreteDistribution"),...))
    else
       {k <- k(x)
        m <- m(x)
        n <- n(x)
        ret.v <- sqrt((m+n-1)/(k*m*n)/(m+n-k))*(n-m)*(m+n-2*k)/(m+n-2)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
        }
    })
### source https://mathworld.wolfram.com/HypergeometricDistribution.html
#
setMethod("skewness", signature(x = "Logis"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
        return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
### source https://mathworld.wolfram.com/LogisticDistribution.html
#
setMethod("skewness", signature(x = "Lnorm"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  {
        return(skewness(as(x,"AbscontDistribution"),...))
    } else {
        w <- exp(sdlog(x)^2)
        ret.v <-  sqrt(w-1)*(w+2)
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/LogNormalDistribution.html
#
setMethod("skewness", signature(x = "Nbinom"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
         return(skewness(as(x,"DiscreteDistribution"),...))
    else{
        p <- prob(x)
        ret.v <- (2-p)/sqrt((1-p)*size(x))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/NegativeBinomialDistribution.html
#
setMethod("skewness", signature(x = "Pois"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
        return(skewness(as(x,"DiscreteDistribution"),...))
    else{
        ret.v <- 1/sqrt(lambda(x))
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
    }
    })
### source https://mathworld.wolfram.com/PoissonDistribution.html
#
setMethod("skewness", signature(x = "Td"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  {
        return(skewness(as(x,"AbscontDistribution"),...))
    } else {
        if (df(x)>3){
        n <- df(x); d<- ncp(x)
        m1 <- E(x)
        m2 <- var(x)
        m3 <- (n/2)^1.5*(3*d+d^3)*exp(lgamma((n-3)/2)-lgamma(n/2))
        ret.v <- (m3-3*m2*m1-m1^3)/m2^1.5
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v)
        } else {
         return(NA)
        }
    }
    })
### source https://mathworld.wolfram.com/NoncentralStudentst-Distribution.html

#
setMethod("skewness", signature(x = "Unif"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
        return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
### source https://mathworld.wolfram.com/UniformDistribution.html
#
setMethod("skewness", signature(x = "Weibull"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
        return(skewness(as(x,"AbscontDistribution"),...))
    else{
        g1 <- gamma(1+1/shape(x))
        g2 <- gamma(1+2/shape(x))
        g3 <- gamma(1+3/shape(x))
        ret.v <-  (g3-3*g1*(g2-g1^2)-g1^3)/(g2-g1^2)^1.5
        if(!propagate.names){names(ret.v) <- NULL}
        return(ret.v) }
    })
### source https://mathworld.wolfram.com/WeibullDistribution.html
#    
setMethod("skewness", signature(x = "Beta"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if((hasArg(fun))||(hasArg(cond))||(!isTRUE(all.equal(ncp(x),0)))) 
        return(skewness(as(x,"AbscontDistribution"),...))
    else
        {a<-shape1(x); b<- shape2(x)
         ret.v <- 2*(b-a)*sqrt(a+b+1)/(a+b+2)/sqrt(a*b)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v) }
    })
## source: https://mathworld.wolfram.com/BetaDistribution.html

###################################################################################
#skewness --- code P.R.:
###################################################################################

setMethod("skewness", signature(x = "Arcsine"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp))  
        return(skewness(as(x,"AbscontDistribution"),...))
    else    return(0)    
    })

