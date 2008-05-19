## Implementation of minimum distance estimation
MDEstimator <- function(x, ParamFamily, distance = KolmogorovDist, dist.name, interval, par, ...){
    res <- MCEstimator(x = x, ParamFamily = ParamFamily, criterion = distance,
                interval = interval, par = par, ...)
    if(missing(dist.name))
      dist.name <- names(distance(x, ParamFamily@distribution))
#    names(res)[2] <- "distance"
#    if(!is.null(dist.name)) names(res$distance) <- dist.name
    if(!is.null(dist.name)) names(res$criterion) <- dist.name
    class(res) <- c("MCEstimate", "Estimate")

    return(res)
}
