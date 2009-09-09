################################################################
# QQ - Plot for distribution objects
# yet to be documented and ranged into distr, distrMod, RobAStBase
################################################################


## into distr
if(!isGeneric("qqplot"))
    setGeneric("qqplot", function(x, y, ...) standardGeneric("qqplot"))
    
## into distr
setMethod("qqplot", signature(x="ANY",y="ANY"), function(x, y,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...){
    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- xlab
    if(missing(ylab)) mc$ylab <- ylab
    mcl <- as.list(mc)[-1]
    do.call(stats::qqplot, args=mcl)
    return(invisible())
    })

## into distr

## helper

.confqq <- function(p,D,alpha,col,lty,n){
   x <- q(D)(p)
   ppq <- sqrt(p*(1-p))
   dp <- d(D)(x)
   alphan <- alpha^(1/n)
   qa <- qnorm((1+alphan)/2)
   lines(x, x+qa*ppq/dp/sqrt(n), col=col,lty=lty)
   lines(x, x-qa*ppq/dp/sqrt(n), col=col,lty=lty)
}
setMethod("qqplot", signature(x = "UnivariateDistribution",
                              y = "UnivariateDistribution"), function(x, y,
                              n = 30, withIdLine = TRUE, withPwConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, alpha.IdL = .95){
    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]
    pp <- ppoints(n)
    xc <- q(x)(pp)
    yc <- q(y)(pp)
    mcl$x <- xc
    mcl$y <- yc
    mcl$n <- NULL
    ret <- do.call(stats::qqplot, args=mcl)
    if(withIdLine){
       abline(0,1,col=col.IdL,lty=lty.IdL)
       if(is(yD,"AbscontDistribution") && withPwConf){
          xy <- sort(c(xc,yc))
          .confqq(p(yD)(xy),yD,alpha.IdL,col.IdL,lty.IdL,length(xy))
       }
    }
    return(ret)
    })
    

## into distrMod
setMethod("qqplot", signature(x = "ANY",
                              y = "UnivariateDistribution"), function(x, y,
                              n = 30, withIdLine = TRUE, withPwConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, alpha.IdL = .95){
    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]
    pp <- ppoints(n)
    yc <- q(y)(pp)
    mcl$y <- yc
    mcl$n <- NULL
    ret <- do.call(stats::qqplot, args=mcl)
    if(withIdLine){
       abline(0,1,col=col.IdL,lty=lty.IdL)
       if(is(yD,"AbscontDistribution") && withPwConf){
          xy <- sort(c(x,yc))
          .confqq(p(yD)(xy),yD,alpha.IdL,col.IdL,lty.IdL,length(xy))
       }
    }
    return(ret)
    })

## into distrMod
setMethod("qqplot", signature(x = "ANY",
                              y = "ProbFamily"), function(x, y,
                              n = 30, withIdLine = TRUE, withPwConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, alpha.IdL = .95){
    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]
    pp <- ppoints(n)
    yD <- y@distribution
    if(!is(yD,"UnivariateDistribution"))
       stop("Not yet implemented.")
    yc <- q(yD)(pp)
    mcl$y <- yc
    mcl$n <- NULL
    ret <- do.call(stats::qqplot, args=mcl)

    if(withIdLine){
       abline(0,1,col=col.IdL,lty=lty.IdL)
       if(is(yD,"AbscontDistribution") && withPwConf){
          xy <- sort(c(x,yc))
          .confqq(p(yD)(xy),yD,alpha.IdL,col.IdL,lty.IdL,length(xy))
       }
    }

    return(ret)
    })

## hier muss noch die Distanz besser gewählt werden:

## into RobAStBase
setMethod("qqplot", signature(x = "ANY",
                              y = "RobModel"), function(x, y,
                              n = length(x), withIdLine = TRUE, withPwConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, alpha.IdL = .95,
    distance = NormType()){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    pp <- ppoints(n)
    yD <- y@center@distribution
    if(!is(yD,"UnivariateDistribution"))
       stop("Not yet implemented.")
    yc <- q(yD)(pp)

    lenx <- n
    leny <- length(x)

    x <- sort(x)

    if (leny < lenx)
        x <- approx(1L:lenx, x, n = leny)$y
    if (leny > lenx)
        yc <- approx(1L:leny, yc, n = lenx)$y

    xD <- fct(distance)(x)
    x.cex <- 3/(1+log(1+xD))

    mcl$x <- x
    mcl$y <- yc
    mcl$n <- NULL
    mcl$cex <- x.cex

    ret <- do.call(stats::qqplot, args=mcl)

    if(withIdLine){
       abline(0,1,col=col.IdL,lty=lty.IdL)
       if(is(yD,"AbscontDistribution") && withPwConf){
          xy <- sort(c(x,yc))
          .confqq(p(yD)(xy),yD,alpha.IdL,col.IdL,lty.IdL,length(xy))
       }
    }

    return(ret)
    })

## into RobAStBase
setMethod("qqplot", signature(x = "ANY",
                              y = "InfRobModel"), function(x, y,
                              n = length(x), withIdLine = TRUE, withPwConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, alpha.IdL = .95,
    distance = NormType()){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    pp <- ppoints(n)
    yD <- y@center@distribution
    if(!is(yD,"UnivariateDistribution"))
       stop("Not yet implemented.")
    yc <- q(yD)(pp)

    lenx <- n
    leny <- length(x)

    x <- sort(x)

    if (leny < lenx)
        x <- approx(1L:lenx, x, n = leny)$y
    if (leny > lenx)
        yc <- approx(1L:leny, yc, n = lenx)$y


    L2D <- L2deriv(y@center)
    FI <- FisherInfo(y@center)
    L2Dx <- sapply(x, function(x) evalRandVar(L2D,x)[[1]])
    scx <-  solve(sqrt(FI),L2Dx)
    xD <- fct(distance)(scx)
    x.cex <- 3/(1+log(1+xD))

    mcl$x <- x
    mcl$y <- yc
    mcl$n <- NULL
    mcl$cex <- x.cex

    ret <- do.call(stats::qqplot, args=mcl)

    if(withIdLine){
       abline(0,1,col=col.IdL,lty=lty.IdL)
       if(is(yD,"AbscontDistribution") && withPwConf){
          xy <- sort(c(x,yc))
          .confqq(p(yD)(xy),yD,alpha.IdL,col.IdL,lty.IdL,length(xy))
       }
    }

    return(ret)
    })
