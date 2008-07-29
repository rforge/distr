###############################################################################
## Function to compute estimates
###############################################################################
Estimator <- function(x, estimator, name, Infos, asvar = NULL, ...){
    if(missing(name))
        name <- "Some estimator"

    if(missing(Infos))
        Infos <- matrix(c(character(0),character(0)), ncol=2,
                        dimnames=list(character(0), c("method", "message")))
    else{
        Infos <- matrix(c(rep("MCEstimator", length(Infos)), Infos), ncol = 2)
        colnames(Infos) <- c("method", "message")
    }

    if(is.null(dim(x)))
         samplesize <- length(x)
    else samplesize <- dim(x)[2]

    new("Estimate", name = name, estimate = estimator(x, ...),
        Infos = Infos, samplesize = samplesize, asvar = asvar)
}
