## initialize method 
setMethod("initialize", "Gumbel",
    function(.Object, loc = 0, scale = 1) {
        .Object@img <- Reals()
        .Object@param <- new("GumbelParameter", loc = loc, scale = scale, 
                             name = gettext("parameter of a Gumbel distribution"))
        .Object@r <- function(n){}
        body(.Object@r) <- substitute({ rgumbel(n, loc = loc1, scale = scale1) },
                                     list(loc1 = loc, scale1 = scale))
        .Object@d <- function(x, log = FALSE){}
        body(.Object@d) <- substitute({  dgumbel(x, loc = loc1, scale = scale1, log = log) },
                                     list(loc1 = loc, scale1 = scale))
        .Object@p <- function(q, lower.tail = TRUE, log.p = FALSE){}
        body(.Object@p) <- substitute({p1 <- pgumbel(q, loc = loc1, scale = scale1, lower.tail = lower.tail) 
                                       return(if(log.p) log(p1) else p1)},
                                     list(loc1 = loc, scale1 = scale))
        .Object@q <- function(p, loc = loc1, scale = scale1, lower.tail = TRUE, log.p = FALSE){}
        body(.Object@q) <- substitute({   
                        ## P.R.: changed to vectorized form 
                        p <- if(log.p) exp(p) else p
                                                                        
                        in01 <- (p>1 | p<0)
                        i01 <- .isEqual01(p) 
                        i0 <- (i01 & p<1)   
                        i1 <- (i01 & p>0)
                        ii01 <- .isEqual01(p) | in01
                                      
                        p0 <- p
                        p0[ii01] <- 0.5
                                      
                        q1 <- qgumbel(p0, loc = loc1, scale = scale1, 
                                      lower.tail = lower.tail) 
                        q1[i0] <- if(lower.tail) -Inf else Inf
                        q1[i1] <- if(!lower.tail) -Inf else Inf
                        q1[in01] <- NaN
                        
                        return(q1)  
                     },  list(loc1 = loc, scale1 = scale))
        .Object@.withSim   <- FALSE
        .Object@.withArith <- FALSE
        .Object@.logExact <- FALSE
        .Object@.lowerExact <- TRUE
        .Object
    })

## Class: Pareto distribution
setMethod("initialize", "Pareto",
          function(.Object, shape = 1, Min = 1, .withArith = FALSE) {
            .Object@img <- new("Reals")
            .Object@param <- new("ParetoParameter", shape = shape, Min =  Min)
            .Object@r <- function(n){}
            .Object@d <- function(x, log = FALSE){}
            .Object@p <- function(q, lower.tail = TRUE, log.p = FALSE){} 
            .Object@q <- function(p, lower.tail = TRUE, log.p = FALSE){} 
            body(.Object@r) <- substitute(
                           { rpareto1(n, shape = shapeSub,  min = MinSub) },
                             list(shapeSub = shape,  MinSub =  Min)
                                       )
            body(.Object@d) <- substitute(
                           { dpareto1(x, shape = shapeSub,  min =  MinSub, 
                                    log = log) },
                             list(shapeSub = shape,  MinSub =  Min)
                                         )
            body(.Object@p) <- substitute(
                           { ppareto1(q, shape = shapeSub,  min =  MinSub, 
                                    lower.tail = lower.tail, log.p = log.p) },
                             list(shapeSub = shape,  MinSub =  Min)
                                         )
            body(.Object@q) <- substitute(
                           { qpareto1(p, shape = shapeSub,  min =  MinSub, 
                                    lower.tail = lower.tail, log.p = log.p) },
                             list(shapeSub = shape,  MinSub =  Min)
                                         )
            .Object@.withArith <- .withArith
            .Object@.logExact <- TRUE
            .Object@.lowerExact <- TRUE
            .Object
          })
