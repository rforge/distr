############################ Expectation

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object){
        I.ac <- E(acPart(object))
        I.dc <- E(discretePart(object))
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "missing"),
    function(object, fun, ... ){
        I.ac <- E(acPart(object), fun = fun, ... )
        I.dc <- E(discretePart(object), fun = fun, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "ANY"),
    function(object, cond, ... ){
        I.ac <- E(acPart(object), cond = cond, ... )
        I.dc <- E(discretePart(object), cond = cond, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "ANY"),
    function(object, fun, cond, ... ){
        I.ac <- E(acPart(object), fun = fun, cond = cond, ... )
        I.dc <- E(discretePart(object), fun = fun, cond = cond, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })

setMethod("E", signature(object = "AffLinUnivarLebDecDistribution",
                         fun = "missing",
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution",
                         fun = "missing",
                         cond = "missing")))

setMethod("E", signature(object = "AcDcLcDistribution",
                         fun = "ANY",
                         cond = "ANY"),
    function(object, fun, cond, ... ){
        object <- distr:::.ULC.cast(object)
        I.ac <- E(acPart(object), fun = fun, cond = cond, ... )
        I.dc <- E(discretePart(object), fun = fun, cond = cond, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })

setMethod("E", signature(object = "CompoundDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object){
         S <- object@SummandsDistr
         N <- object@NumbOfSummandsDistr
       if(is(S,"UnivariateDistribution"))
          return(E(S)*E(N))
       else{
          return(E(simplifyD(object)))
       }
    })
