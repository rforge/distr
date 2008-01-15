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
