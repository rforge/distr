###############################################################################
## Method: CvMDist
## Cramer - von Mises distance of two distributions
###############################################################################
setMethod("CvMDist", signature(e1 = "UnivariateDistribution",
                                    e2 = "UnivariateDistribution"),
    function(e1, e2, mu = e2, useApply = FALSE, ... ){
        owarn <- getOption("warn"); options(warn = -1)
        res <- E(mu, fun = function(t) {(p(e1)(t)-p(e2)(t))^2}, useApply = useApply, ...)^.5
        names(res) <- "CvM distance"
        options(warn = owarn)
        return(res)
    })

## CvM distance
setMethod("CvMDist", signature(e1 = "numeric",
                                    e2 = "UnivariateDistribution"),
    function(e1, e2, mu = e2, ...)
        {owarn <- getOption("warn"); options(warn = -1)
          e10 <- DiscreteDistribution(e1)       
          options(warn = owarn)
          CvMDist(e1 = e10, e2 = e2, mu = mu, ...)
         }
    )

