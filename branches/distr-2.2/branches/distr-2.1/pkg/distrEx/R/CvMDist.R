###############################################################################
## Method: CvMDist
## Cramer - von Mises distance of two distributions
###############################################################################
setMethod("CvMDist", signature(e1 = "UnivariateDistribution",
                                    e2 = "UnivariateDistribution"),
    function(e1, e2, mu = e2, useApply = FALSE, ... ){
        o.warn <- getOption("warn"); options(warn = -1)
        on.exit(options(warn=o.warn))
        if(is.null(e1@p)){
        e1.erg <- RtoDPQ(e1@r)
        e1 <- new("UnivariateDistribution", r=e1@r, 
                   p = e1.erg$pfun, d = e1.erg$dfun, q = e1.erg$qfun,  
                   .withSim = TRUE, .withArith = FALSE)}
        if(is.null(e2@p)){
        e2.erg <- RtoDPQ(e2@r)
        e2 <- new("UnivariateDistribution", r=e2@r, 
                   p = e2.erg$pfun, d = e2.erg$dfun, q = e2.erg$qfun,  
                   .withSim = TRUE, .withArith = FALSE)}
        res <- E(mu, fun = function(t) {(p(e1)(t)-p(e2)(t))^2}, useApply = useApply, ...)^.5
        names(res) <- "CvM distance"
        return(res)
    })

## CvM distance
setMethod("CvMDist", signature(e1 = "numeric",
                                    e2 = "UnivariateDistribution"),
    function(e1, e2, mu = e2, ...)
        { o.warn <- getOption("warn"); options(warn = -1)
          on.exit(options(warn=o.warn))
          e10 <- DiscreteDistribution(e1)       
          CvMDist(e1 = e10, e2 = e2, mu = mu, ...)
         }
    )

