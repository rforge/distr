###############################################################################
## Method: HellingerDist
## Hellinger distance of two distributions
###############################################################################
setMethod("HellingerDist", signature(e1 = "AbscontDistribution", 
                                     e2 = "AbscontDistribution"),
    function(e1, e2){
        TruncQuantile <- getdistrOption("TruncQuantile")  
        lower1 <- ifelse(is.nan(q(e1)(0)), q(e1)(TruncQuantile), q(e1)(0))
        upper1 <- ifelse(is.nan(q(e1)(1)), 
                         ifelse("lower.tail" %in% names(formals(e1@q)),
                                q(e1)(TruncQuantile, lower.tail = FALSE),
                                q(e1)(1-TruncQuantile)), 
                         q(e1)(1))
        lower2 <- ifelse(is.nan(q(e2)(0)), q(e2)(TruncQuantile), q(e2)(0))
        upper2 <- ifelse(is.nan(q(e2)(1)), 
                         ifelse("lower.tail" %in% names(formals(e2@q)),
                                q(e2)(TruncQuantile, lower.tail = FALSE),
                                q(e2)(1-TruncQuantile)), 
                         q(e2)(1))
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
