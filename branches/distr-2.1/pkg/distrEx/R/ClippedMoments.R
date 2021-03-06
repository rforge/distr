###############################################################################
## Clipped first and second moments
###############################################################################
setMethod("m1df", "UnivariateDistribution",
    function(object, upper){
        x <- r(object)(.distrExOptions$MCIterations)
        return(mean(x*(x<=upper)))
    })
setMethod("m2df", "UnivariateDistribution",
    function(object, upper){
        x <- r(object)(.distrExOptions$MCIterations)
        return(mean(x^2*(x<=upper)))
    })
setMethod("m1df", "AbscontDistribution",
    function(object, upper){
        integrandm1 <- function(x, dfun){ x * dfun(x) }
        return(distrExIntegrate(integrandm1, lower = q(object)(.distrExOptions$m1dfLowerTruncQuantile), 
                    rel.tol = .distrExOptions$m1dfRelativeTolerance, upper = upper, dfun = d(object), 
                    distr = object))
    })
setMethod("m2df", "AbscontDistribution",
    function(object, upper){
        integrandm2 <- function(x, dfun){ x^2 * dfun(x) }
        return(distrExIntegrate(integrandm2, lower = q(object)(.distrExOptions$m2dfLowerTruncQuantile), 
                    rel.tol = .distrExOptions$m2dfRelativeTolerance, upper = upper, dfun = d(object), 
                    distr = object))
    })
setMethod("m1df", "DiscreteDistribution",
    function(object, upper){
        supp <- support(object)
        supp <- supp[supp <= upper]
        dfun <- d(object)
        return(sum(supp * dfun(supp)))
    })
setMethod("m2df", "DiscreteDistribution",
    function(object, upper){
        supp <- support(object)
        supp <- supp[supp <= upper]
        dfun <- d(object)
        return(sum(supp^2 * dfun(supp)))
    })
setMethod("m1df", "Binom",
    function(object, upper){
        return(prob(object)*size(object)*pbinom(upper-1, prob = prob(object),
                                                size = size(object)-1))
    })
setMethod("m2df", "Binom",
    function(object, upper){
        n <- size(object)
        p <- prob(object)
        return(n*p*(pbinom(upper-1, prob = p, size = n-1) 
               + p*(n-1)*pbinom(upper-2, prob = p, size = n-2)))
    })
setMethod("m1df", "Pois",
    function(object, upper){
        return(lambda(object)*ppois(upper-1, lambda = lambda(object)))
    })
setMethod("m2df", "Pois",
    function(object, upper){
        lam <- lambda(object)
        return(lam*(ppois(upper-1, lambda = lam) + lam*ppois(upper-2, lambda = lam)))
    })
setMethod("m1df", "Norm",
    function(object, upper){
        mu <- mean(object)
        std <- sd(object)
        return(mu*pnorm((upper-mu)/std) - std*dnorm((upper-mu)/std))
    })
setMethod("m2df", "Norm",
    function(object, upper){
        mu <- mean(object)
        std <- sd(object)
        if(abs(pnorm((upper-mu)/std)-1) > .Machine$double.eps)
            return((mu^2+std^2)*pnorm((upper-mu)/std) 
                    - std*(upper + mu)*dnorm((upper-mu)/std))
        else
            return(mu^2+std^2)
    })
setMethod("m1df", "Exp",
    function(object, upper){
        if(upper <= 0) return(0)
        lam <- rate(object)
        if(abs(pexp(lam*upper)-1) > .Machine$double.eps)
            return(pexp(lam*upper)/lam - upper*exp(-lam*upper))
        else
            return(1/lam)
    })
setMethod("m2df", "Exp",
    function(object, upper){
        if(upper <= 0) return(0)
        lam <- rate(object)
        if(abs(pexp(lam*upper)-1) > .Machine$double.eps)
            return(2*pexp(lam*upper)/lam^2 
                    - (upper + 2/lam)*upper*exp(-lam*upper))
        else
            return(2/lam^2)
    })
setMethod("m1df", "Chisq",
    function(object, upper){
        ncp <- ncp(object)
        if(ncp != 0)
            if(abs(p(object)(upper)-1) > .Machine$double.eps)
                return(m1df(as(object, "AbscontDistribution"), upper))
            else
                return(dfr + ncp)
        dfr <- df(object)
        return(dfr*pchisq(upper, df = (dfr+2)))
    })
setMethod("m2df", "Chisq",
    function(object, upper){
        ncp <- ncp(object)
        if(ncp != 0)
            if(abs(p(object)(upper)-1) > .Machine$double.eps)
                return(m2df(as(object, "AbscontDistribution"), upper))
            else
                return(dfr^2 + 2*dfr*(ncp+1) + ncp*(ncp + 4))
        dfr <- df(object)
        return(dfr*(dfr+2)*pchisq(upper, df = (dfr+4)))
    })
setMethod("m1df", "LatticeDistribution",
    function(object, upper){
        getMethod("m1df", "DiscreteDistribution")(
              as(object, "DiscreteDistribution"), upper)
    })
setMethod("m2df", "LatticeDistribution",
    function(object, upper){
        getMethod("m2df", "DiscreteDistribution")(
              as(object, "DiscreteDistribution"), upper)
    })

setMethod("m1df", "AffLinDistribution", 
    function(object, upper){
             a <- object@a
             b <- object@b
             if(a>0)
                a * m1df(object@X0,(upper-b)/a) + b
             else   
                (-a) * m1df(-object@X0,(upper-b)/(-a)) + b
    })
