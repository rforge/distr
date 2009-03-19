############################ Expectation

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ...){
        I.ac <- E(acPart(object), rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object))
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "missing"),
    function(object, fun, useApply = TRUE, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        I.ac <- E(acPart(object), fun = fun, useApply = useApply, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), fun = fun, useApply = useApply, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "ANY"),
    function(object, cond, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        I.ac <- E(acPart(object), cond = cond, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), cond = cond, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "ANY"),
    function(object, fun, cond, useApply = TRUE, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        I.ac <- E(acPart(object), fun = fun, cond = cond, useApply = useApply, 
                  rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), fun = fun, cond = cond, 
                  useApply = useApply, ... )
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
    function(object, fun, cond, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        object <- distr:::.ULC.cast(object)
        I.ac <- E(acPart(object), fun = fun, cond = cond, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), fun = fun, cond = cond, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })

setMethod("E", signature(object = "CompoundDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, ...){
         S <- object@SummandsDistr
         N <- object@NumbOfSummandsDistr
       if(is(S,"UnivariateDistribution"))
          return(E(S, ...)*E(N))
       else{
          return(E(simplifyD(object), ...))
       }
    })
