############################################################################################
##
## distances between (empirical) data and distributions
##
############################################################################################

## Kolmogorov distance
setMethod("KolmogorovDist", signature(e1 = "numeric",
                                      e2 = "UnivariateDistribution"),
    function(e1, e2){
        owarn <- options(warn = -1)
        res <- ks.test(e1, e2@p)$statistic
        options(owarn)
        names(res) <- "Kolmogorov distance"
        return(res)
    })

setMethod("KolmogorovDist", signature(e1 = "UnivariateDistribution",
                                      e2 = "numeric"),
    function(e1, e2){
        return(KolmogorovDist(e2, e1))
    })


## Hellinger distance
setMethod("HellingerDist", signature(e1 = "numeric",
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){
        d1 <- table(e1)/length(e1)
        d2 <- d(e2)(sort(unique(e1)))
        e21 <- setdiff(support(e2), unique(e1))
        d21 <- d(e2)(e21)
        res <- sqrt(1/2)*sqrt(sum((sqrt(d1)-sqrt(d2))^2) + sum(d21))
        names(res) <- "Hellinger distance"
        return(res)
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution",
                                     e2 = "numeric"),
    function(e1, e2){
        return(HellingerDist(e2, e1))
    })

## to avoid trivial distances (distance = 1)
## abs.cont. distributions may be discretized
## resp. empirical distributions may be smoothed
## (by convolution with a normal distribution)
setMethod("HellingerDist", signature(e1 = "numeric",
                                     e2 = "AbscontDistribution"),
    function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrModOption("nDiscretize"), low.discr = getLow(e2),
             up.discr = getUp(e2), h.smooth = getdistrModOption("hSmooth")){
        .asis.smooth.discretize.distance(e1, e2, asis.smooth.discretize, n.discr,
                 low.discr, up.discr, h.smooth, HellingerDist)
    })
setMethod("HellingerDist", signature(e1 = "AbscontDistribution",
                                     e2 = "numeric"),
    function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrModOption("nDiscretize"), low.discr = getLow(e1),
             up.discr = getUp(e1), h.smooth = getdistrModOption("hSmooth")){
        return(HellingerDist(e2, e1, asis.smooth.discretize = asis.smooth.discretize, 
                  low.discr = low.discr, up.discr = up.discr, h.smooth = h.smooth))
    })


## total variation distance
setMethod("TotalVarDist", signature(e1 = "numeric",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){
        d1 <- table(e1)/length(e1)
        d2 <- d(e2)(sort(unique(e1)))
        e21 <- setdiff(support(e2), unique(e1))
        d21 <- d(e2)(e21)
        res <- 1/2*(sum(abs(d2-d1))+sum(d21))
        names(res) <- "Total variation distance"
        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "numeric"),
    function(e1, e2){
        return(TotalVarDist(e2, e1))
    })

## to avoid trivial distances (distance = 1)
## abs.cont. distributions may be discretized
## resp. empirical distributions may be smoothed 
## (by convolution with a normal distribution)
setMethod("TotalVarDist", signature(e1 = "numeric",
                                    e2 = "AbscontDistribution"),
     function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrModOption("nDiscretize"), low.discr = getLow(e2),
             up.discr = getUp(e2), h.smooth = getdistrModOption("hSmooth")){
        .asis.smooth.discretize.distance(e1, e2, asis.smooth.discretize, n.discr,
                 low.discr, up.discr, h.smooth, TotalVarDist)
     })
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution",
                                     e2 = "numeric"),
    function(e1, e2, asis.smooth.discretize = "discretize", n.discr =
             getdistrModOption("nDiscretize"), low.discr = getLow(e1),
             up.discr = getUp(e1), h.smooth = getdistrModOption("hSmooth")){
        return(TotalVarDist(e2, e1, asis.smooth.discretize = asis.smooth.discretize, 
                  low.discr = low.discr, up.discr = up.discr, h.smooth = h.smooth))
    })

