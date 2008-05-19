## initialize method 
setMethod("initialize", "Gumbel",
    function(.Object, loc = 0, scale = 1) {
        .Object@img <- Reals()
        .Object@param <- new("GumbelParameter", loc = loc, scale = scale, 
                             name = gettext("parameter of a Gumbel distribution"))
        .Object@r <- function(n){ rgumbel(n, loc = loc1, scale = scale1) }
        body(.Object@r) <- substitute({ rgumbel(n, loc = loc1, scale = scale1) },
                                     list(loc1 = loc, scale1 = scale))
        .Object@d <- function(x, ...){ dgumbel(x, loc = loc1, scale = scale1, ...) }
        body(.Object@d) <- substitute({ dgumbel(x, loc = loc1, scale = scale1, ...) },
                                     list(loc1 = loc, scale1 = scale))
        .Object@p <- function(x, ...){ pgumbel(x, loc = loc1, scale = scale1, ...) }
        body(.Object@p) <- substitute({ pgumbel(x, loc = loc1, scale = scale1, ...) },
                                     list(loc1 = loc, scale1 = scale))
        .Object@q <- function(x, ...){ 
                        if(x == 0) return(-Inf)
                        if(x == 1) return(Inf)
                        qgumbel(x, loc = loc1, scale = scale1, ...) 
                     }
        body(.Object@q) <- substitute({ if(x == 0) return(-Inf)
                                        if(x == 1) return(Inf)
                                        qgumbel(x, loc = loc1, scale = scale1, ...) },
                                     list(loc1 = loc, scale1 = scale))
        .Object@.withSim   <- FALSE
        .Object@.withArith <- FALSE
        .Object
    })
