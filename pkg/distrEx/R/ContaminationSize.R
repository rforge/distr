###############################################################################
## Method: ContaminationSize
## size of contamination for two distributions
###############################################################################
setMethod("ContaminationSize", signature(e1 = "AbscontDistribution", 
                                         e2 = "AbscontDistribution"),
    function(e1, e2){
        ep <- getdistrOption("TruncQuantile")
        lower <- min(q(e1)(ep), q(e2)(ep))
        upper <- max(q(e1)(1-ep), q(e2)(1-ep))
        x <- seq(from = lower, to = upper, length = 1e5)
        
        d10  <- d(e1)(x); d1 <- d10[ d10>0 ]
        d20  <- d(e2)(x); d2 <- d20[ d10>0 ]
        res <- 1 - min(d2/d1)
        return(list(e1 = e1, e2 = e2, size.of.contamination = res))
    })

setMethod("ContaminationSize", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution"),
    function(e1, e2){
        owarn <- getOption("warn"); options(warn = -1)
        x <- union(support(e1), support(e2))
        d10  <- d(e1)(x); d1 <- d10[ d10>0 ]
        d20  <- d(e2)(x); d2 <- d20[ d10>0 ]
        
        res <- min(1- min(d2/d1),1)

        options(warn = owarn)


        return(list(e1 = e1, e2 = e2, size.of.contamination = res))
    })
