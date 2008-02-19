###############################################################################
## Method: vonMisesDist
## von Mises distance of two distributions
###############################################################################
setMethod("vonMisesDist", signature(e1 = "UnivariateDistribution",
                                    e2 = "UnivariateDistribution"),
    function(e1, e2, mu = e2, ... ){
        owarn <- getOption("warn"); options(warn = -1)
        res <- E(mu, fun = function(t) {(p(e1)(t)-p(e2)(t))^2}, useApply = FALSE, ...)^.5
        names(res) <- "vonMises distance"
        options(warn = owarn)
        return(res)
    })

## vonMises distance
setMethod("vonMisesDist", signature(e1 = "numeric",
                                    e2 = "UnivariateDistribution"),
    function(e1, e2, mu = e2, ...)
        {owarn <- getOption("warn"); options(warn = -1)
          e10 <- DiscreteDistribution(e1)       
          options(warn = owarn)
          vonMisesDist(e1 = e10, e2 = e2, mu = mu, ...)
         }
    )

