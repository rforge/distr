###############################################################################
## Implementation of minimum distance estimation
###############################################################################
MDEstimator <- function(x, ParamFamily, distance = KolmogorovDist, dist.name, 
                        interval, par, Infos, ...){
    res <- MCEstimator(x = x, ParamFamily = ParamFamily, criterion = distance,
                interval = interval, par = par, Infos = Infos, ...)
    if(missing(dist.name))
      dist.name <- names(distance(x, ParamFamily@distribution))

    names(res@criterion) <- dist.name
    res@name <- paste("Minimum", dist.name, "estimate", sep = " ")

    return(res)
}

