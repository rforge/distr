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
        .Object@q <- function(p, loc = loc1, scale = scale1, ...){}
        body(.Object@q) <- substitute({   
                        ## P.R.: changed to vectorized form 
                        mc <-  as.list(match.call(call = sys.call())[-1])
                        lower.tail <- mc$lower.tail                                      
                        if(is.null(lower.tail)) lower.tail <- TRUE
                        
                        in01 <- (p>1 | p<0)
                        i01 <- .isEqual01(p) 
                        i0 <- (i01 & p<1)   
                        i1 <- (i01 & p>0)
                        ii01 <- .isEqual01(p) | in01
                                      
                        p0 <- p
                        p0[ii01] <- 0.5
                                      
                        q1 <- qgumbel(p0, loc = loc1, scale = scale1, ...) 
                        q1[i0] <- if(lower.tail) -Inf else Inf
                        q1[i1] <- if(!lower.tail) -Inf else Inf
                        q1[in01] <- NaN
                        
                        return(q1)  
                     },  list(loc1 = loc, scale1 = scale))
        .Object@.withSim   <- FALSE
        .Object@.withArith <- FALSE
        .Object
    })
