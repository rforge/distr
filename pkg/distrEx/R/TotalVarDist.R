###############################################################################
## Method: TotalVarDist
## total variation distance of two distributions
###############################################################################
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution", 
                                    e2 = "AbscontDistribution"),
    function(e1, e2){
        TruncQuantile <- getdistrOption("TruncQuantile")  
        lower1 <- ifelse(!is.finite(q(e1)(0)), q(e1)(TruncQuantile), q(e1)(0))
        upper1 <- ifelse(!is.finite(q(e1)(1)), 
                         ifelse("lower.tail" %in% names(formals(e1@q)),
                                q(e1)(TruncQuantile, lower.tail = FALSE),
                                q(e1)(1-TruncQuantile)), 
                         q(e1)(1))
        lower2 <- ifelse(!is.finite(q(e2)(0)), q(e2)(TruncQuantile), q(e2)(0))
        upper2 <- ifelse(!is.finite(q(e2)(1)), 
                         ifelse("lower.tail" %in% names(formals(e2@q)),
                                q(e2)(TruncQuantile, lower.tail = FALSE),
                                q(e2)(1-TruncQuantile)), 
                         q(e2)(1))
        lower <- min(lower1, lower2)
        upper <- max(upper1, upper2)

        o.warn <- getOption("warn"); options(warn = -1)
        on.exit(options(warn=o.warn))
        integrand <- function(x, dfun1, dfun2){ 0.5*abs(dfun1(x)-dfun2(x)) }
        res <- distrExIntegrate(integrand, lower = lower, upper = upper, 
                    dfun1 = d(e1), dfun2 = d(e2), rel.tol=.Machine$double.eps^0.3)
        names(res) <- "total variation distance"

        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){
        o.warn <- getOption("warn"); options(warn = -1)
        on.exit(options(warn=o.warn))
        supp <- union(support(e1), support(e2))

        res <- 0.5*sum(abs(d(e1)(supp)-d(e2)(supp)))
        names(res) <- "total variation distance"

        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "AbscontDistribution"),
    function(e1, e2){
        res <- 1
        names(res) <- "total variation distance"

        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){ 
        res <- 1
        names(res) <- "total variation distance"

        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "numeric",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){
        d1 <- table(e1)/length(e1)
        d2 <- d(e2)(sort(unique(e1)))
        e21 <- setdiff(support(e2), unique(e1))
        d21 <- d(e2)(e21)
        res <- 1/2*(sum(abs(d2-d1))+sum(d21))
        names(res) <- "Total variation distance"

        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "numeric"),
    function(e1, e2){
        return(TotalVarDist(e2, e1))
    })

## to avoid trivial distances (distance = 1)
## abs.cont. distributions may be discretized
## resp. empirical distributions may be smoothed 
## (by convolution with a normal distribution)
setMethod("TotalVarDist", signature(e1 = "numeric",
                                    e2 = "AbscontDistribution"),
     function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrExOption("nDiscretize"), low.discr = getLow(e2),
             up.discr = getUp(e2), h.smooth = getdistrExOption("hSmooth")){
        .asis.smooth.discretize.distance(e1, e2, asis.smooth.discretize, n.discr,
                 low.discr, up.discr, h.smooth, TotalVarDist)
     })
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution",
                                     e2 = "numeric"),
    function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrExOption("nDiscretize"), low.discr = getLow(e1),
             up.discr = getUp(e1), h.smooth = getdistrExOption("hSmooth")){
        return(TotalVarDist(e2, e1, asis.smooth.discretize = asis.smooth.discretize, 
                  low.discr = low.discr, up.discr = up.discr, h.smooth = h.smooth))
    })
#### new from version 2.0 on: Distance for Mixing Distributions
setMethod("TotalVarDist",  signature(e1 = "AcDcLcDistribution",
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
              res <- TotalVarDist(ac1,ac2) + TotalVarDist(dc1,dc2)
              names(res) <- "total variation distance"
              res
              })
setMethod("TotalVarDist", signature(e1 = "LatticeDistribution", 
                                         e2 = "LatticeDistribution"),
    getMethod("TotalVarDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution")))

setMethod("TotalVarDist", signature(e1 = "LatticeDistribution", 
                                         e2 = "DiscreteDistribution"),
    getMethod("TotalVarDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution")))

setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "LatticeDistribution"),
    getMethod("TotalVarDist", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution")))
