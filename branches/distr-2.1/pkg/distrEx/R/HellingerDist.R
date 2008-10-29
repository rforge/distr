###############################################################################
## Method: HellingerDist
## Hellinger distance of two distributions
###############################################################################
setMethod("HellingerDist", signature(e1 = "AbscontDistribution", 
                                     e2 = "AbscontDistribution"),
    function(e1, e2){
        TruncQuantile <- getdistrOption("TruncQuantile")  
        lower1 <- getLow(e1)
        upper1 <- getUp(e1)
        lower2 <- getLow(e2)
        upper2 <- getUp(e2)
        lower <- min(lower1, lower2)
        upper <- max(upper1, upper2)

        owarn <- getOption("warn"); options(warn = -1)
        integrand <- function(x, dfun1, dfun2){ 0.5*(sqrt(dfun1(x))-sqrt(dfun2(x)))^2 }
        res <- distrExIntegrate(integrand, lower = lower, upper = upper, 
                    dfun1 = d(e1), dfun2 = d(e2), rel.tol=.Machine$double.eps^0.3)
        names(res) <- "Hellinger distance"
        options(warn = owarn)

        return(sqrt(res))  # ^.5 added P.R. 19-12-06
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){
        owarn <- getOption("warn"); options(warn = -1)
        supp <- union(support(e1), support(e2))

        res <- 0.5*sum((sqrt(d(e1)(supp))-sqrt(d(e2)(supp)))^2)  
        names(res) <- "Hellinger distance"
        options(warn = owarn)

        return(sqrt(res)) # ^.5 added P.R. 19-12-06
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                     e2 = "AbscontDistribution"),
    function(e1, e2){
        res <- 1
        names(res) <- "Hellinger distance"

        return(res)
    })
setMethod("HellingerDist", signature(e1 = "AbscontDistribution", 
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){ 
        res <- 1
        names(res) <- "Hellinger distance"

        return(res)
    })
## Hellinger distance
setMethod("HellingerDist", signature(e1 = "numeric",
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){
        d1 <- table(e1)/length(e1)
        d2 <- d(e2)(sort(unique(e1)))
        e21 <- setdiff(support(e2), unique(e1))
        d21 <- d(e2)(e21)
        res <- sqrt(1/2)*sqrt(sum((sqrt(d1)-sqrt(d2))^2) + sum(d21))
        names(res) <- "Hellinger distance"

        return(res)
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution",
                                     e2 = "numeric"),
    function(e1, e2){
        return(HellingerDist(e2, e1))
    })

## to avoid trivial distances (distance = 1)
## abs.cont. distributions may be discretized
## resp. empirical distributions may be smoothed
## (by convolution with a normal distribution)
setMethod("HellingerDist", signature(e1 = "numeric",
                                     e2 = "AbscontDistribution"),
    function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrExOption("nDiscretize"), low.discr = getLow(e2),
             up.discr = getUp(e2), h.smooth = getdistrExOption("hSmooth")){
        .asis.smooth.discretize.distance(e1, e2, asis.smooth.discretize, n.discr,
                 low.discr, up.discr, h.smooth, HellingerDist)
    })
setMethod("HellingerDist", signature(e1 = "AbscontDistribution",
                                     e2 = "numeric"),
    function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrExOption("nDiscretize"), low.discr = getLow(e1),
             up.discr = getUp(e1), h.smooth = getdistrExOption("hSmooth")){
        return(HellingerDist(e2, e1, asis.smooth.discretize = asis.smooth.discretize, 
                  low.discr = low.discr, up.discr = up.discr, h.smooth = h.smooth))
    })

#### new from version 2.0 on: Distance for Mixing Distributions
setMethod("HellingerDist", signature(e1 = "AcDcLcDistribution",
                                     e2 = "AcDcLcDistribution"),
           function(e1, e2){
           if( is(e1,"AbscontDistribution"))
               e1 <- as(as(e1,"AbscontDistribution"), "UnivarLebDecDistribution")
           if( is(e2,"AbscontDistribution"))
               e2 <- as(as(e2,"AbscontDistribution"), "UnivarLebDecDistribution")
           if(is(e1,"DiscreteDistribution"))
               e1 <- as(as(e1,"DiscreteDistribution"), "UnivarLebDecDistribution")
           if(is(e2,"DiscreteDistribution"))
               e2 <- as(as(e2,"DiscreteDistribution"), "UnivarLebDecDistribution")
              ac1 <- acPart(e1); ac2 <- acPart(e2)
              ac1d <- ac1@d; ac2d <- ac2@d
              ac1@d <- function(x) ac1d(x)*acWeight(e1)
              ac2@d <- function(x) ac2d(x)*acWeight(e2)
              dc1 <- discretePart(e1); dc2 <- discretePart(e2)
              dc1d <- dc1@d; dc2d <- dc2@d
              dc1@d <- function(x) dc1d(x)*discreteWeight(e1)
              dc2@d <- function(x) dc2d(x)*discreteWeight(e2)
              da2 <- HellingerDist(ac1,ac2)^2
              dd2 <- HellingerDist(dc1,dc2)^2
              res <- (da2+dd2)^.5
              names(res) <- "Hellinger distance"
              res
})

setMethod("HellingerDist", signature(e1 = "LatticeDistribution", 
                                         e2 = "LatticeDistribution"),
    getMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution")))

setMethod("HellingerDist", signature(e1 = "LatticeDistribution", 
                                         e2 = "DiscreteDistribution"),
    getMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution")))

setMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "LatticeDistribution"),
    getMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution")))
