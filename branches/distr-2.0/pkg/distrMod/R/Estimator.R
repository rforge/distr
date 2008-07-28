###############################################################################
## Determine estimator
###############################################################################
Estimator <- function(x, estimator, name, Infos, ...){
    if(missing(name))
        name <- "Some estimator"

    if(missing(Infos))
        Infos <- matrix(c(character(0),character(0)), ncol=2,
                        dimnames=list(character(0), c("method", "message")))
    else{
        Infos <- matrix(c(rep("MCEstimator", length(Infos)), Infos), ncol = 2)
        colnames(Infos) <- c("method", "message")
    }

    new("Estimate", name = name, estimate = estimator(x, ...),
        Infos = Infos)
}
