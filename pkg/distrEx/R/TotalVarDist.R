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

        owarn <- getOption("warn"); options(warn = -1)
        integrand <- function(x, dfun1, dfun2){ 0.5*abs(dfun1(x)-dfun2(x)) }
        res <- distrExIntegrate(integrand, lower = lower, upper = upper, 
                    dfun1 = d(e1), dfun2 = d(e2), rel.tol=.Machine$double.eps^0.3) 
        options(warn = owarn)

        return(list(e1 = e1, e2 = e2, total.variation.distance = res))
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){
        owarn <- getOption("warn"); options(warn = -1)
        supp <- union(support(e1), support(e2))

        res <- 0.5*sum(abs(d(e1)(supp)-d(e2)(supp)))
        options(warn = owarn)

        return(list(e1 = e1, e2 = e2, total.variation.distance = res))
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "AbscontDistribution"),
    function(e1, e2){ 
        return(list(e1 = e1, e2 = e2, total.variation.distance = 1))
    })
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){ 
        return(list(e1 = e1, e2 = e2, total.variation.distance = 1))
    })
## total variation distance
setMethod("TotalVarDist", signature(e1 = "numeric",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){
        d1 <- table(e1)/length(e1)
        d2 <- d(e2)(sort(unique(e1)))
        e21 <- setdiff(support(e2), unique(e1))
        d21 <- d(e2)(e21)
        res <- 1/2*(sum(abs(d2-d1))+sum(d21))
        names(res) <- "Total variation distance"
        return(list(e1 = e1, e2 = e2, total.variation.distance = res))
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
             getdistrModOption("nDiscretize"), low.discr = getLow(e2),
             up.discr = getUp(e2), h.smooth = getdistrModOption("hSmooth")){
        .asis.smooth.discretize.distance(e1, e2, asis.smooth.discretize, n.discr,
                 low.discr, up.discr, h.smooth, TotalVarDist)
     })
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution",
                                     e2 = "numeric"),
    function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrModOption("nDiscretize"), low.discr = getLow(e1),
             up.discr = getUp(e1), h.smooth = getdistrModOption("hSmooth")){
        return(TotalVarDist(e2, e1, asis.smooth.discretize = asis.smooth.discretize, 
                  low.discr = low.discr, up.discr = up.discr, h.smooth = h.smooth))
    })
