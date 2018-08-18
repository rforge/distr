
MultivarMixingDistribution <- function(..., Dlist, mixCoeff #,
#                                     withSimplify = getdistrOption("simplifyD")
                                     )
   {
    ldots <- list(...)
    if(!missing(Dlist)){
        Dlist.L <- as(Dlist, "list")
        if(!is(try(do.call(MultivarDistrList,args=Dlist.L),silent=TRUE),"try-error"))
            ldots <- c(ldots, Dlist.L)
       }
    l <- length(ldots)
    mixDistr <- do.call(MultivarDistrList,args=ldots)
    if(is(mixDistr,"UnivarDistrList"))
       return(UnivarMixingDistribution(Dlist = mixDistr, mixCoeff = mixCoeff))
    ep <- .Machine$double.eps
    if(missing(mixCoeff))
       mixCoeff <- rep(1,l)/l
    else{ if (l!=length(mixCoeff))
          stop("argument 'mixCoeff' and the mixing distributions must have the same length")
          if(any(mixCoeff < -ep) || sum(mixCoeff)>1+ep)
             stop("mixing coefficients are no probabilities")
        }
    rnew <- .rMVmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)

    .check <- function(Dlist,slotname){
          ret <- TRUE
          for(i in seq(length = length(Dlist)))
              if(is.null(slot(Dlist[[i]], slotname))) ret <- FALSE
          return(ret)       
    }
    pnew <- NULL
    if(.check(mixDistr,"p"))
       pnew <- .pMVmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)


    .withArith <- any(as.logical(lapply(mixDistr, function(x) x@".withArith")))
    .withSim   <- any(as.logical(lapply(mixDistr, function(x) x@".withSim")))
    .lowerExact<- all(as.logical(lapply(mixDistr, function(x) x@".lowerExact")))

    if (all( as.logical(lapply(mixDistr, function(x) is(x,"AbscontDistribution")))) ||
        all( as.logical(lapply(mixDistr, function(x) is(x,"DiscreteDistribution")))))
    dnew <- NULL
    if(.check(mixDistr,"d"))
        dnew <- .dMVmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)

    
    qnew <- NULL

    obj <- new("MultivarMixingDistribution", p = pnew, r = rnew, d = NULL, q = qnew,
         mixCoeff = mixCoeff, mixDistr = mixDistr, .withSim = .withSim,
         .withArith = .withArith,.lowerExact =.lowerExact)

    if (all( as.logical(lapply(mixDistr, function(x) is(x@Symmetry,"SphericalSymmetry"))))){
       sc <- SymmCenter(mixDistr[[1]]@Symmetry) 
       if (all( as.logical(lapply(mixDistr, function(x) .isEqual(SymmCenter(x@Symmetry),sc)))))
           obj@Symmetry <- SphericalSymmetry(sc)    
    }
    
    
#    if (withSimplify)
#        obj <- simplifyD(obj)

    return(obj)
}
#mylist2 <- MultivarMixingDistribution(Binom(3,.3), mylist,
#          mixCoeff=c(.3,.7))

setMethod("dim", "MultivarMixingDistribution", function(x)x@mixDistr[[1]]@img@dimension)
setMethod("dimension", "MultivarMixingDistribution", function(object)object@mixDistr[[1]]@img@dimension)

setMethod("mixCoeff", "MultivarMixingDistribution", function(object)object@mixCoeff)
setReplaceMethod("mixCoeff", "MultivarMixingDistribution", function(object,value){
   object@mixCoeff<- value; object})


setMethod("mixDistr", "MultivarMixingDistribution", function(object)object@mixDistr)
setReplaceMethod("mixDistr", "MultivarMixingDistribution", function(object,value){
   object@mixDistr<- value; object})


setMethod("Symmetry", "MultivarMixingDistribution",
           function(object)slot(object, "Symmetry"))

.rMVmixfun <- function(mixDistr, mixCoeff){
  l <- length(mixCoeff)
  return(function(n){
  Un <- sample(1:l, size = n, replace = TRUE, prob = mixCoeff)
  mal <- lapply(mixDistr, function(x) x@r(n))
  ma <- array(unlist(mal), dim=c(dim(mixDistr),n,l))
  sapply(1:n, function(i) ma[,i,Un[i]])
  })
}
.pMVmixfun <- function(mixDistr, mixCoeff){
  l <- length(mixCoeff)
  return(function(lower=-Inf, upper=Inf, log.p = FALSE){
  p0 <- as.vector(
           matrix(unlist(
              lapply(mixDistr, 
                  function(x)
                       do.call(x@p, list(lower = lower, upper = upper)) 
              )),
              ncol = l, nrow = length(q)) %*% mixCoeff
           )
  if(log.p) p0 <- log(p0)
  return(p0)               
   })
}
.dMVmixfun <- function(mixDistr, mixCoeff, withStand = FALSE, supp = NULL){
  l <- length(mixCoeff)
  if(withStand) {su <- sum(as.vector(matrix(unlist(lapply(mixDistr, function(y)
                                               do.call(y@d, list(x = supp)))),
                                 ncol = l, nrow = length(supp)) %*% mixCoeff))
                }else su <- 1
 
  return(function(x, log = FALSE ){
         d0 <- 
         as.vector(matrix(unlist(lapply(mixDistr, function(y)
            do.call(y@d, list(x = x)))),
         ncol = l, nrow = length(x)) %*% mixCoeff)/su
         if(log) d0 <- log(d0)
         return(d0)
         })
}


setMethod("E", signature(object = "MultivarMixingDistribution",
                        fun = "missing", cond = "missing"),
                        function(object, ..., diagnostic=FALSE) {
             l <- length(object@mixCoeff)
             dotsI <- .filterEargs(list(...))
             diagn <- vector("list",l)
             res0 <- do.call(E, c(list(object=object@mixDistr[[1]], diagnostic=diagnostic), dotsI))
             diagn[[1]] <- attr(res0,"diagnostic")
             res <- object@mixCoeff[1]*res0
             diagn[["call"]] <- match.call()
             if(diagnostic) attr(res,"diagnostic") <- diagn
             if(l==1) return(res)
             for(i in 2:l){
                 res0 <-  do.call(E, c(list(object=object@mixDistr[[i]], diagnostic=diagnostic), dotsI))
                 diagn[[i]] <- attr(res0,"diagnostic")
                 res <- res + object@mixCoeff[i]*res0
             }
             if(diagnostic){
                attr(res,"diagnostic") <- diagn
                class(attr(res,"diagnostic"))<- "DiagnosticClass"
             }
             return(res)
           })
setMethod("E", signature(object = "MultivarMixingDistribution",
                        fun = "function", cond = "missing"), 
                        function(object, fun, ..., diagnostic=FALSE) {
             l <- length(object@mixCoeff)
             dots <- list(...)
             dotsI <- .filterEargs(dots)
             dotsFun <- .filterFunargs(dots,fun)
             funwD <- function(x) do.call(fun, c(list(x), dotsFun))
             diagn <- vector("list",l)
             res0 <-  do.call(E, c(list(object=object@mixDistr[[1]], fun = funwD, diagnostic=diagnostic), dotsI))
             diagn[[1]] <- attr(res0,"diagnostic")
             res <- object@mixCoeff[1]*res0
             diagn[["call"]] <- match.call()
             if(diagnostic) attr(res,"diagnostic") <- diagn
             if(l==1) return(res)
             for(i in 2:l){
                 res0 <-  do.call(E, c(list(object=object@mixDistr[[i]], fun = funwD, diagnostic=diagnostic), dotsI))
                 diagn[[i]] <- attr(res0,"diagnostic")
                 res <- res + object@mixCoeff[i]*res0
             }
             if(diagnostic){
                attr(res,"diagnostic") <- diagn
                class(attr(res,"diagnostic"))<- "DiagnosticClass"
             }
             return(res)
           })

setMethod("var", signature(x = "MultivarMixingDistribution"),
           function(x,...){            
             l <- length(x@mixCoeff)
             if(l==1L) return(var(x@mixDistr[[1]],...))
             E1 <- E2 <- 0
             for(i in 1:l){
                E10 <- E(x@mixDistr[[i]], ...)
                E1 <- E1 + x@mixCoeff[i]*E10
                E2 <- E2 + (E10%*%t(E10)+var(x@mixDistr[[i]],...)) * 
                            x@mixCoeff[i]
           }
           return(E2-E1%*%t(E1))})


setMethod("plot", signature(x = "MultivarMixingDistribution", y = "missing"),
      function(x, Nsim = getdistrEllipseOption("Nsim"), ...,
               withED = getdistrEllipseOption("withED"),
               lwd.Ed = getdistrEllipseOption("lwd.Ed"),
               col.Ed = getdistrEllipseOption("col.Ed"),
               withMean = getdistrEllipseOption("withMean"),
               cex.mean = getdistrEllipseOption("cex.mean"),
               pch.mean = getdistrEllipseOption("pch.mean"),
               col.mean = getdistrEllipseOption("col.mean")){
      mc <- as.list(match.call(call = sys.call(sys.parent(1)),
                         expand.dots = FALSE))[-1]
      do.call(getMethod("plot", signature(x = "SphericalDistribution", y = "missing")),
              args = mc)
      } )

setMethod("show", "MultivarMixingDistribution",
          function(object){
            cls <- class(object)[1]
            cat(showobj(object, className = cls))
            ws <- .IssueWarn(object@.withArith, object@.withSim)
            if(!is.null(ws$msgA)) warning(ws$msgA)
            if(!is.null(ws$msgS)) warning(ws$msgS)
          }
         )


setMethod("showobj", "MultivarMixingDistribution",
          function(object, className = class(object)[1]){
              txt <- gettextf("An object of class \"%s\"\n", className)
              l <- length(mixCoeff(object))
              txt <- c(txt,
                     "---------------------------------------------\n")
              txt <- c(txt,
                  gettextf("It consists of  %i components \n", l))
              txt <- c(txt,
                     gettextf("Components: \n"))

              for(i in 1:l){
                 s <- showobj(mixDistr(object)[[i]])
                 les <- length(s)
                 st <- if (les>1)
                     c(gettextf("[[%i]]", i), rep("      :",les-1))
                 else  gettextf("[[%i]]", i)
                 txt <- c(txt,
                          paste(st, gettextf("%s",s),sep=""))}
              txt <- c(txt,
                     "---------------------------------------------\n")
              txt <- c(txt,
                          gettextf("Weights: \n"))
              txt <- c(txt, gettextf("%f",
                          round(mixCoeff(object),3)))
            txt<-c(txt,"\n ---------------------------------------------\n")
            return(txt)
            }
          )
