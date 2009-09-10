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

## helpers
.q2kolmogorov <- function(alpha,n,exact=(n<100)){ ## Kolmogorovstat
 if(exact){
 fct <- function(p0){
 ### from ks.test from package stats:
    .C("pkolmogorov2x", p = as.double(p0),
       as.integer(n), PACKAGE = "stats")$p -alpha
  }
 res <- uniroot(fct,lower=0,upper=1)$root*sqrt(n)
 }else{
 ### from ks.test from package stats:
 pkstwo <- function(x, tol = 1e-06) {
        if (is.numeric(x))
            x <- as.vector(x)
        else stop("argument 'x' must be numeric")
        p <- rep(0, length(x))
        p[is.na(x)] <- NA
        IND <- which(!is.na(x) & (x > 0))
        if (length(IND)) {
            p[IND] <- .C("pkstwo", as.integer(length(x[IND])),
                p = as.double(x[IND]), as.double(tol), PACKAGE = "stats")$p
        }
        return(p)
    }
 ###
 fct <- function(p0){
      1 - pkstwo(p0)-alpha  }
 res <- uniroot(fct,lower=0,upper=sqrt(n))$root
 }
 return(res)
}

.BinomCI <- function(p,n,alpha){
  fct <- function(t,p1.bi){
   pbinom(q = pmin(n*p1.bi+t*sqrt(n),n+1), size = n, prob = p1.bi) -
   pbinom(q = pmax(n*p1.bi-t*sqrt(n),-1), size = n, prob = p1.bi)-alpha
  }
  sapply(p, function(p2) uniroot(fct, lower=0, upper=sqrt(n)+1,p1.bi=p2,tol=1e-9)$root)
}

.confqq <- function(x,D,alpha,col.pCI,lty.pCI,lwd.pCI,col.sCI,lty.sCI,lwd.sCI,
                    n,exact=(n<100)){
   p <- p(D)(x)
   c.crit <- .q2kolmogorov(alpha,n,exact)
   c.crit.i <- .BinomCI(p,n,alpha)

   lines(x, q(D)(pmin(p+c.crit.i/sqrt(n),1-1e-9)),
         col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
   lines(x, q(D)(pmax(p-c.crit.i/sqrt(n),0+1e-9)),
         col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)

   lines(x, q(D)(pmin(p+c.crit/sqrt(n),1-1e-9)),
         col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
   lines(x, q(D)(pmax(p-c.crit/sqrt(n),0+1e-9)),
         col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
   legend("topleft", legend = eval(substitute(expression(
      "pointw. exact"~alpha==alpha0~"%- conf. interval",
      "simult."~ex0~alpha==alpha0~"%- conf. interval"#,
      ),list(ex0=if(exact) "exact" else "asympt.",
             alpha0=alpha*100))), lty=c(lty.pCI,lty.sCI), col=c(col.pCI,col.sCI),
            lwd=2,cex=.8)
}

.deleteItemsMCL <- function(mcl){
    mcl$n <- NULL
    mcl$col.IdL <- mcl$alpha.CI <- mcl$lty.IdL <- mcl$exact.sCI <- NULL
    mcl$withConf <- mcl$withIdLine <- mcl$distance <- NULL
    mcl$col.pCI <- mcl$lty.pCI <- mcl$col.sCI <- mcl$lty.sCI <- NULL
    mcl$lwd.IdL <- mcl$lwd.pCI <- mcl$lwd.sCI <- NULL
    mcl$withLab <- mcl$lab.pts <- mcl$which.lbs <- NULL
    mcl$which.Order <- mcl$order.traf  <- NULL
    mcl$col.pch <- mcl$cex.pch  <- NULL
    mcl$col.lbl <- mcl$cex.lbl  <- mcl$adj.lbl <- NULL
mcl}

.labelprep <- function(x,y,lab.pts,which.lbs,which.Order,order.traf){
      n <- length(x)
      xys <- cbind(x,y[rank(x)])
      if(is.null(which.lbs)) which.lbs <- 1:n
      oN0 <- order(xys[,1],decreasing=TRUE)
      if(!is.null(order.traf)){
          oN0 <- order(order.traf(xys[,1]),decreasing=TRUE)
      }
      oN0b <- oN0 %in% which.lbs
      oN0 <- oN0[oN0b]
      oN <- oN0
      if(!is.null(which.Order))
          oN <- oN0[which.Order]
      x0 <- xys[oN,1]
      y0 <- xys[oN,2]
      if(is.null(lab.pts)) lab.pts <- paste(oN)
      else {lab.pts <-  rep(lab.pts, length.out=n)
            lab.pts <- lab.pts[oN]}
      return(list(x0=x0,y0=y0,lab=lab.pts))
}

setMethod("qqplot", signature(x = "UnivariateDistribution",
                              y = "UnivariateDistribution"), function(x, y,
                              n = 30, withIdLine = TRUE, withConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, lwd.IdL = 2,
    alpha.CI = .95, exact.sCI = (n<100),
    col.pCI = "orange", lty.pCI = 3, lwd.pCI = 2,
    col.sCI = "tomato2", lty.sCI = 4, lwd.sCI = 2,
    cex.pch=par("cex"), col.pch = par("col")){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    pp <- ppoints(n)
    xc <- q(x)(pp)
    yc <- q(y)(pp)

    mcl$x <- xc
    mcl$y <- yc

    mcl <- .deleteItemsMCL(mcl)
    mcl$cex <- cex.pch
    mcl$col <- col.pch

    ret <- do.call(stats::qqplot, args=mcl)

    if(withIdLine&& plot.it){
       abline(0,1,col=col.IdL,lty=lty.IdL,lwd=lwd.IdL)
       if(#is(y,"AbscontDistribution") &&
       withConf){
          xy <- sort(c(xc,yc))
          .confqq(xy, y, alpha.CI, col.pCI, lty.pCI, lwd.pCI,
                      col.sCI, lty.sCI, lwd.sCI,
                  length(xc), exact = exact.sCI)
       }
    }
    return(ret)
    })
    

## into distrMod
setMethod("qqplot", signature(x = "ANY",
                              y = "UnivariateDistribution"),
    function(x,    ### observations
             y,    ### distribution
             n = length(x), ### number of points to be plotted
             withIdLine = TRUE, ### shall line y=x be plotted in
             withConf = TRUE,   ### shall confidence lines be plotted
             plot.it = TRUE,    ### shall be plotted at all (inherited from stats::qqplot)
             xlab = deparse(substitute(x)), ## x-label
             ylab = deparse(substitute(y)), ## y-label
             ...,                 ## further parameters
             withLab = TRUE,      ## shall observation labels be plotted in
             lab.pts = NULL,      ## observation labels to be used
             which.lbs = NULL,    ## which observations shall be labelled
             which.Order = NULL,  ## which of the ordered (remaining) observations shall be labelled
             order.traf = NULL,   ## an optional trafo; by which the observations are ordered (as order(trafo(obs))
             col.IdL = "red",     ## color for the identity line
             lty.IdL = 2,         ## line type for the identity line
             lwd.IdL = 2,         ## line width for the identity line
             alpha.CI = .95,      ## confidence level
             exact.sCI = (n<100), ## shall simultaneous CIs be determined with exact kolmogorov distribution?
             col.pCI = "orange",  ## color for the pointwise CI
             lty.pCI = 3,         ## line type for the pointwise CI
             lwd.pCI = 2,         ## line width for the pointwise CI
             col.sCI = "tomato2", ## color for the simultaneous CI
             lty.sCI = 4,         ## line type for the simultaneous CI
             lwd.sCI = 2,         ## line width for the simultaneous CI
             cex.pch = par("cex"),## magnification factor for the plotted symbols
             col.pch = par("col"),## color for the plotted symbols
             cex.lbl = par("cex"),## magnification factor for the plotted observation labels
             col.lbl = par("col"),## color for the plotted observation labels
             adj.lbl = NULL       ## adj parameter for the plotted observation labels
    ){ ## return value as in stats::qqplot

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]


    pp <- ppoints(n)
    yc <- q(y)(pp)

    if(n!=length(x)) withLab <- FALSE


    mcl$y <- yc
    mcl <- .deleteItemsMCL(mcl)
    mcl$cex <- cex.pch
    mcl$col <- col.pch


    ret <- do.call(stats::qqplot, args=mcl)

    if(withLab&& plot.it){
      lbprep <- .labelprep(x,yc,lab.pts,which.lbs,which.Order,order.traf)
       text(x = lbprep$x0, y = lbprep$y0, labels = lbprep$lab,
            cex = cex.lbl, col = col.lbl, adj = adj.lbl)
    }

    if(withIdLine&& plot.it){
       abline(0,1,col=col.IdL,lty=lty.IdL,lwd=lwd.IdL)
       if(#is(y,"AbscontDistribution")&&
       withConf){
          xy <- sort(c(x,yc))
          .confqq(xy, y, alpha.CI, col.pCI, lty.pCI, lwd.pCI,
                  col.sCI, lty.sCI, lwd.sCI,
                  length(x), exact = exact.sCI)
       }
    }
    return(ret)
    })

## into distrMod
setMethod("qqplot", signature(x = "ANY",
                              y = "ProbFamily"), function(x, y,
                              n = length(x), withIdLine = TRUE, withConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    withLab = TRUE, lab.pts = NULL,
    which.lbs = NULL, which.Order = NULL, order.traf = NULL,
    col.IdL = "red", lty.IdL = 2, lwd.IdL = 2,
    alpha.CI = .95, exact.sCI = (n<100),
    col.pCI = "orange", lty.pCI = 3, lwd.pCI = 2,
    col.sCI = "tomato2", lty.sCI = 4, lwd.sCI = 2,
    cex.pch=par("cex"), col.pch = par("col"),
    cex.lbl = par("cex"), col.lbl = par("col"),
    cex.pch=par("cex"), col.pch = par("col"),
    cex.lbl = par("cex"), col.lbl = par("col")
    ){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    mcl$y <- yD <- y@distribution
    if(!is(yD,"UnivariateDistribution"))
       stop("Not yet implemented.")

    return(do.call(getMethod("qqplot", signature(x="ANY", y="UnivariateDistribution")),
            args=mcl))
    })

## hier muss noch die Distanz besser gewählt werden:

## into RobAStBase
setMethod("qqplot", signature(x = "ANY",
                              y = "RobModel"), function(x, y,
                              n = length(x), withIdLine = TRUE, withConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    withLab = TRUE, lab.pts = NULL,
    which.lbs = NULL, which.Order = NULL, order.traf = NULL,
    col.IdL = "red", lty.IdL = 2, lwd.IdL = 2,
    alpha.CI = .95, exact.sCI = (n<100),
    col.pCI = "orange", lty.pCI = 3, lwd.pCI = 2,
    col.sCI = "tomato2", lty.sCI = 4, lwd.sCI = 2,
    col.pch = par("col"),
    cex.lbl = par("cex"), col.lbl = par("col"),
    distance = NormType()){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    mcl$y <- y@center

    xD <- fct(distance)(x)
    x.cex <- 3/(1+log(1+xD))
    mcl$cex.pch <- x.cex

    return(do.call(getMethod("qqplot", signature(x="ANY", y="ProbFamily")),
            args=mcl))
    })

## into RobAStBase
setMethod("qqplot", signature(x = "ANY",
                              y = "InfRobModel"), function(x, y,
                              n = length(x), withIdLine = TRUE, withConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    withLab = TRUE, lab.pts = NULL,
    which.lbs = NULL, which.Order = NULL, order.traf = NULL,
    col.IdL = "red", lty.IdL = 2, lwd.IdL = 2,
    alpha.CI = .95, exact.sCI = (n<100),
    col.pCI = "orange", lty.pCI = 3, lwd.pCI = 2,
    col.sCI = "tomato2", lty.sCI = 4, lwd.sCI = 2,
    col.pch = par("col"),
    cex.lbl = par("cex"), col.lbl = par("col"),
    distance = NormType()){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    mcl$y <- y@center

    L2D <- L2deriv(y@center)
    FI <- PosSemDefSymmMatrix(FisherInfo(y@center))
    L2Dx <- sapply(x, function(z) evalRandVar(L2D,z)[[1]])
    scx <-  solve(sqrt(FI),matrix(L2Dx,ncol=length(x)))
    xD <- fct(distance)(scx)
    x.cex <- 3/(1+log(1+xD))
    mcl$cex.pch <- x.cex

    return(do.call(getMethod("qqplot", signature(x="ANY", y="ProbFamily")),
            args=mcl))
    })
