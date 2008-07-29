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
        .Object@p <- function(q, ...){ pgumbel(q, loc = loc1, scale = scale1, ...) }
        body(.Object@p) <- substitute({ pgumbel(q, loc = loc1, scale = scale1, ...) },
                                     list(loc1 = loc, scale1 = scale))
        .Object@q <- function(p, ...){ 
                        ## P.R.: changed to vectorized form 
                        p0 <- p
                        p0[.isEqual01(p)] <- 0.5
                        q0 <- qgumbel(p0, loc = loc1, scale = scale1, ...)
                        q0[.isEqual01(p)] <- sign(p[.isEqual01(p)]-0.5)*Inf
                        return(q0)  
                     }
        body(.Object@q) <- substitute({
                              ## P.R.: changed to vectorized form 
                              p0 <- p
                              p0[.isEqual01(p)] <- 0.5
                              q0 <- qgumbel(p0, loc = loc1, 
                                            scale = scale1, ...)
                              q0[.isEqual01(p)] <- sign(p[.isEqual01(p)]-0.5)*Inf
                              return(q0)
                               },
                              list(loc1 = loc, scale1 = scale))
        .Object@.withSim   <- FALSE
        .Object@.withArith <- FALSE
        .Object
    })
