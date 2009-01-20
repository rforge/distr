UnivarMixingDistribution <- function(..., Dlist, mixCoeff,
                                     withSimplify = getdistrOption("simplifyD"))
   {
    ldots <- list(...)
    l <- length(ldots)
    l0 <- 0
    if(!missing(Dlist)){
        Dlist <- as(Dlist, "list")
        if(!is(try(do.call(UnivarDistrList,args = Dlist),"try-error")))
            ldots <- c(ldots, Dlist)
       }
    l <- l + l0
    mixDistr <- new("UnivarDistrList", ldots)
    ep <- .Machine$double.eps
    if(missing(mixCoeff))
       mixCoeff <- rep(1,l)/l
    else{ if (l!=length(mixCoeff))
          stop("argument 'mixCoeff' and the mixing distributions must have the same length")
          if(any(mixCoeff < -ep) || sum(mixCoeff)>1+ep)
             stop("mixing coefficients are no probabilities")
        }
    rnew <- .rmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)

    pnew <- .pmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)


    .withArith <- any(as.logical(lapply(mixDistr, function(x) x@".withArith")))
    .withSim   <- any(as.logical(lapply(mixDistr, function(x) x@".withSim")))
    .lowerExact<- all(as.logical(lapply(mixDistr, function(x) x@".lowerExact")))

    if (all( as.logical(lapply(mixDistr, function(x) is(x,"AbscontDistribution")))) ||
        all( as.logical(lapply(mixDistr, function(x) is(x,"DiscreteDistribution")))))
        dnew <- .dmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)


    qnew <- .qmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff,
                     Cont = TRUE, pnew = pnew)

    obj <- new("UnivarMixingDistribution", p = pnew, r = rnew, d = NULL, q = qnew,
         mixCoeff = mixCoeff, mixDistr = mixDistr, .withSim = .withSim,
         .withArith = .withArith,.lowerExact =.lowerExact)

    if (withSimplify)
        obj <- simplifyD(obj)

    return(obj)
}


setMethod("mixCoeff", "UnivarMixingDistribution", function(object)object@mixCoeff)
setReplaceMethod("mixCoeff", "UnivarMixingDistribution", function(object,value){
   object@mixCoeff<- value; object})


setMethod("mixDistr", "UnivarMixingDistribution", function(object)object@mixDistr)
setReplaceMethod("mixDistr", "UnivarMixingDistribution", function(object,value){
   object@mixDistr<- value; object})
