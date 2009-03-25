############################ Expectation

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ...){
        if(is(Symmetry(object),"SphericalSymmetry"))
           return(SymmCenter(Symmetry(object)))
        I.ac <- E(acPart(object), low = low, upp = upp, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), low = low, upp = upp )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "missing"),
    function(object, fun, useApply = TRUE, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        I.ac <- E(acPart(object), fun = fun, useApply = useApply, 
                  low = low, upp = upp, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), fun = fun, useApply = useApply, 
                  low = low, upp = upp, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })
setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "missing",
                         cond = "ANY"),
    function(object, cond, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        I.ac <- E(acPart(object), cond = cond, low = low, upp = upp, 
                  rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), cond = cond, low = low, upp = upp, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })

setMethod("E", signature(object = "UnivarLebDecDistribution",
                         fun = "function",
                         cond = "ANY"),
    function(object, fun, cond, useApply = TRUE, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        I.ac <- E(acPart(object), fun = fun, cond = cond, useApply = useApply, 
                  low = low, upp = upp, rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), fun = fun, cond = cond, 
                  useApply = useApply, low = low, upp = upp, ... )
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
    function(object, fun, cond, low = NULL, upp = NULL, 
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"), ... ){
        object <- distr:::.ULC.cast(object)
        I.ac <- E(acPart(object), fun = fun, cond = cond, low = low, upp = upp, 
                  rel.tol = rel.tol, 
                  lowerTruncQuantile = lowerTruncQuantile,
                  upperTruncQuantile = upperTruncQuantile,
                  IQR.fac = IQR.fac, ... )
        I.dc <- E(discretePart(object), fun = fun, cond = cond, low = low, 
                  upp = upp, ... )
        as.vector(object@mixCoeff %*% c(I.ac, I.dc))
    })

setMethod("E", signature(object = "CompoundDistribution",
                         fun = "missing",
                         cond = "missing"),
    function(object, low = NULL, upp = NULL, ...){
         S <- object@SummandsDistr
         N <- object@NumbOfSummandsDistr
        if(!is.null(low)) if(low <= q(object)(0)) low <- NULL
        if(!is.null(upp)) if(upp >= q(object)(1)) upp <- NULL
 
       if(is(S,"UnivariateDistribution") && is.null(low) && is.null(upp))
          return(E(S, ...)*E(N))
       else{
          return(E(simplifyD(object), low = low, upp = upp, ...))
       }
    })
