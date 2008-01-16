## Implementation of minimum distance estimation
MDEstimator <- function(x, ParamFamily, distance = KolmogorovDist, interval, par, ...){
    res <- MCEstimator(x = x, ParamFamily = ParamFamily, criterion = distance,
                interval = interval, par = par, ...)
    dist.name <- names(distance(x, ParamFamily@distribution))
    if(is.null(dist.name))
      names(res)[2] <- "distance"
    else
      names(res)[2] <- dist.name
    class(res) <- c("MDEstimator", "MCEstimator")

    return(res)
}
