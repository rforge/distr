############################ Expectation

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, fun, cond){
        I.ac <- E(acPart(object))
        I.dc <- E(discretePart(object))
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "missing"),
    function(object, fun, cond, ... ){
        I.ac <- E(acPart(object), fun = fun, ... )
        I.dc <- E(discretePart(object), fun = fun, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "ANY"),
    function(object, fun, cond, ... ){
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

