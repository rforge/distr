###############################################################################
## Method: KolmogorovDist
## Kolmogorov distance of two distributions
###############################################################################
setMethod("KolmogorovDist", signature(e1 = "AbscontDistribution",
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
        x1 <- union(r(e1)(1e5), r(e2)(1e5))
        x2 <- seq(from=lower, to=upper, length=1e5)
        x <- union(x1, x2) 

        res <- max(abs(p(e1)(x)-p(e2)(x)))
        options(warn = owarn)


        return(list(e1 = e1, e2 = e2, Kolmogorov.distance = res))
    })

setMethod("KolmogorovDist", signature(e1 = "DiscreteDistribution",
                                      e2 = "DiscreteDistribution"),
    function(e1, e2){
        owarn <- getOption("warn"); options(warn = -1)
        supp <- union(support(e1), support(e2))

        res <- max(abs(p(e1)(supp)-p(e2)(supp)))
        options(warn = owarn)


        return(list(e1 = e1, e2 = e2, Kolmogorov.distance = res))
    })

setMethod("KolmogorovDist", signature(e1 = "DiscreteDistribution",
                                      e2 = "AbscontDistribution"),
    function(e1, e2){
        TruncQuantile <- getdistrOption("TruncQuantile")
        lower <- ifelse(!is.finite(q(e2)(0)), q(e2)(TruncQuantile), q(e2)(0))
        upper <- ifelse(!is.finite(q(e2)(1)), 
                         ifelse("lower.tail" %in% names(formals(e2@q)),
                                q(e2)(TruncQuantile, lower.tail = FALSE),
                                q(e2)(1-TruncQuantile)), 
                         q(e2)(1))

        owarn <- getOption("warn"); options(warn = -1)
        x1 <- union(support(e1), r(e2)(1e5))
        x2 <- seq(from=lower, to=upper, length=1e5)
        x <- union(x1, x2) 

        res <- max(abs(p(e1)(x)-p(e2)(x)))
        options(warn = owarn)

        return(list(e1 = e1, e2 = e2, Kolmogorov.distance = res))
    })

setMethod("KolmogorovDist", signature(e1 = "AbscontDistribution",
                                      e2 = "DiscreteDistribution"),
    function(e1, e2){
        KolmogorovDist(e2, e1)
    })
## Kolmogorov distance
setMethod("KolmogorovDist", signature(e1 = "numeric",
                                      e2 = "UnivariateDistribution"),
    function(e1, e2){
        owarn <- options(warn = -1)
        res <- ks.test(e1, e2@p)$statistic
        options(owarn)
        names(res) <- "Kolmogorov distance"
        return(list(e1 = e1, e2 = e2, "Kolmogorov distance" = res)
    })

setMethod("KolmogorovDist", signature(e1 = "UnivariateDistribution",
                                      e2 = "numeric"),
    function(e1, e2){
        return(KolmogorovDist(e2, e1))
    })
