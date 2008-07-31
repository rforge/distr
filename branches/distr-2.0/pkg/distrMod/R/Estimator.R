###############################################################################
## Function to compute estimates
###############################################################################
Estimator <- function(x, estimator, name, Infos, asvar = NULL, nuis.idx,
                      ...){
    if(missing(name))
        name <- "Some estimator"

    if(missing(Infos))
        Infos <- matrix(c(character(0),character(0)), ncol=2,
                        dimnames=list(character(0), c("method", "message")))
    else{
        Infos <- matrix(c(rep("MCEstimator", length(Infos)), Infos), ncol = 2)
        colnames(Infos) <- c("method", "message")
    }

    samplesize <- if(is.null(dim(x))) length(x) else dim(x)[2]

    if(missing(nuis.idx)) nuis.idx <- NULL

    new("Estimate", name = name, estimate = estimator(x, ...),
        Infos = Infos, samplesize = samplesize, asvar = asvar,
        nuis.idx = nuis.idx)
}
