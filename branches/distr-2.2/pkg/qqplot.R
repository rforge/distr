################################################################
# QQ - Plot for distribution objects
# yet to be documented and ranged into distr, distrMod, RobAStBase
################################################################

### to be written into the respective MASKING files....

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
.inGaps <- function(x,gapm){
  fct <- function(x,m){ m[,2]>=x & m[,1]<=x}
  sapply(x, function(y) length(which(fct(y,gapm)))>0)
}

.isReplicated <- function(x){
  tx <- table(x)
  rx <- as.numeric(names(tx[tx>1]))
  sapply(x, function(y) any(abs(y-rx)<.Machine$double.eps))
}

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
 ###  end of code from package stats
 fct <- function(p0){
      1 - pkstwo(p0)-alpha  }
 res <- uniroot(fct,lower=0,upper=sqrt(n))$root
 }
 return(res)
}

.BinomCI <- function(x,p.b,D,n,alpha){
  if(length(x)==0) return(NA)
  fct <- function(t,p.bi,x.i){
   p.bi.u <- p(D)(x.i+t/sqrt(n))
   p.bi.l <- p.l(D)(x.i-t/sqrt(n))
   d.r <- if(n*p.bi>floor(n*p.bi)) 0 else
        dbinom(x = n*p.bi, size = n, prob = pmax(p.bi.u,1))
   p.r <- pbinom(q = n*p.bi, size = n, prob = pmin(p.bi.u,1),lower.tail=FALSE)+d.r
   p.l <- pbinom(q = n*p.bi, size = n, prob = pmax(p.bi.l,0),lower.tail=FALSE)
   r <- p.r -p.l - alpha
#   print(c(r=r,p=p.bi,x=x.i,p.u=p.bi.u,p.l=p.bi.l,r.r=p.r,r.l=p.l,t=t,np=n*p.bi))
   r
  }
  sapply(1:length(x), function(i) uniroot(fct, lower=0, upper=sqrt(n)*max(x[i],n-x[i]),
         p.bi = p.b[i], x.i = x[i], tol = 1e-9)$root)
}
.q2pw <- function(x,p.b,D,n,alpha,exact=(n<100)){
 if(exact)
    return(.BinomCI(x,p.b,D,n,alpha))
 pq <- p.b*(1-p.b)
 if(is(D,"AbscontDistribution")){
    dp <- d(D)(x)
 }else{
    supp.ind <- sapply(x, function(y)
                 which.min(abs(y-support(D))))
    nx <- length(support(D))
    supp.ind.p <- pmax(supp.ind + 1 ,nx)
    supp.ind.m <- pmax(supp.ind - 1 ,1)
    dsupp.p <- support(D)[supp.ind.p] - support(D)[supp.ind]
    dsupp.m <- support(D)[supp.ind] - support(D)[supp.ind.m]
    s <- sd(D)
    m <- E(D)
#    print(c(pq[1:3], x[1:3],dsupp.p[1:3],dsupp.m[1:3],m,s))
    dp <- pnorm((x+dsupp.p/2-m)/s) - pnorm((x-dsupp.m/2-m)/s)
 }
 return(sqrt(pq)/dp*qnorm((1+alpha)/2))
}

.confqq <- function(x,D,alpha,col.pCI,lty.pCI,lwd.pCI,col.sCI,lty.sCI,lwd.sCI,
                    n,exact.sCI=(n<100),exact.pCI=(n<100)){
   p.r <- p(D)(x)
   p.l <- p.l(D)(x)
   c.crit <- #q.r(D)(p(D)(
             .q2kolmogorov(alpha,n,exact.sCI)
             #))
   c.crit.i <- #q.r(D)(p(D)(
              .q2pw(x,p.r,D,n,alpha,exact.pCI)
              #))
#   print(c.crit.i)
   lines(x, x+c.crit.i/sqrt(n),
         col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
   lines(x, x-c.crit.i/sqrt(n),
         col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)

   lines(x, q.r(D)(pmax(1-p.r-c.crit/sqrt(n),getdistrOption("DistrResolution")),lower.tail=FALSE),
         col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
   lines(x, q(D)(pmax(p.l-c.crit/sqrt(n),getdistrOption("DistrResolution"))),
         col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
   legend("topleft", legend = eval(substitute(expression(
      "symm. pointw."~ex.p~alpha==alpha0~"%- conf. interval",
      "simult."~ex.s~alpha==alpha0~"%- conf. interval"#,
      ),list(ex.s=if(exact.sCI) "exact" else "asympt.",
             ex.p=if(exact.pCI) "exact" else "asympt.",
             alpha0=alpha*100))), lty=c(lty.pCI,lty.sCI), col=c(col.pCI,col.sCI),
            lwd=2,cex=.8)
}

.deleteItemsMCL <- function(mcl){
    mcl$n <- NULL
    mcl$col.IdL <- mcl$alpha.CI <- mcl$lty.IdL <-  NULL
    mcl$col.NotInSupport <- mcl$check.NotInSupport <- NULL
    mcl$exact.sCI <- mcl$exact.pCI <- NULL
    mcl$withConf <- mcl$withIdLine <- mcl$distance <- NULL
    mcl$col.pCI <- mcl$lty.pCI <- mcl$col.sCI <- mcl$lty.sCI <- NULL
    mcl$lwd.IdL <- mcl$lwd.pCI <- mcl$lwd.sCI <- NULL
    mcl$withLab <- mcl$lab.pts <- mcl$which.lbs <- NULL
    mcl$which.Order <- mcl$order.traf  <- NULL
    mcl$col.pch <- mcl$cex.pch  <- mcl$jit.fac <- NULL
    mcl$col.lbl <- mcl$cex.lbl  <- mcl$adj.lbl <- NULL
mcl}

## helper into distrMod
.labelprep <- function(x,y,lab.pts,col.lbl,which.lbs,which.Order,order.traf){
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
      return(list(x0=x0,y0=y0,lab=lab.pts,col=col.lbl[oN]))
}

## into distr:
setMethod("qqplot", signature(x = "UnivariateDistribution",
                              y = "UnivariateDistribution"), function(x, y,
                              n = 30, withIdLine = TRUE, withConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, lwd.IdL = 2,
    alpha.CI = .95, exact.sCI = (n<100), exact.pCI = (n<100),
    col.pCI = "orange", lty.pCI = 3, lwd.pCI = 2,
    col.sCI = "tomato2", lty.sCI = 4, lwd.sCI = 2,
    cex.pch=par("cex"), col.pch = par("col"),
    jit.fac = 0, check.NotInSupport = TRUE,
    col.NotInSupport = "red"){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]
    force(x)

    pp <- ppoints(n)
    xc <- q(x)(pp)
    yc <- q(y)(pp)

    col.pch <- rep(col.pch,length.out=n)

    if(check.NotInSupport){
       xco <- sort(xc)
       nInSupp <- which(xco < q(y)(0))
       nInSupp <- unique(sort(c(nInSupp,which( ! xco > q(y)(1)))))
       if("support" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( ! xco %in% support(y)))))
       if("gaps" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( .inGaps(xco,gaps(y))))))
       if(length(nInSupp)){
          col.pch[nInSupp] <- col.NotInSupport
       }
    }


    oxc <- 1:length(xc)
    xc.o <- xc
    yc.o <- yc

    if("support" %in% names(getSlots(class(x)))){
       xc <- jitter(xc, factor=jit.fac)
       oxc <- order(xc)
       xc <- xc[oxc]
       col.nInSupp <- col.nInSupp [oxc]
       }

    if("support" %in% names(getSlots(class(y))))
       yc <- sort(jitter(yc, factor=jit.fac))

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
          xy <- unique(sort(c(xc.o,yc.o)))
          lxy <- length(xy)
          if(lxy<n){
             xy0 <- seq(min(xy),max(xy),length=n-lxy)
             xy <- sort(c(xy,xy0))
          }
          .confqq(xy, y, alpha.CI, col.pCI, lty.pCI, lwd.pCI,
                      col.sCI, lty.sCI, lwd.sCI,
                  length(xc), exact.sCI = exact.sCI, exact.pCI = exact.pCI)
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
             exact.pCI = (n<100), ## shall pointwise CIs be determined with exact Binomial distribution?
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
             adj.lbl = NULL,      ## adj parameter for the plotted observation labels
             jit.fac = 0,         ## jittering factor used for discrete distributions
             check.NotInSupport = TRUE, ## shall we check if all x lie in support(y)
             col.NotInSupport = "red" ## if preceding check TRUE color of x if not in support(y)
    ){ ## return value as in stats::qqplot

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]
    force(x)

    xj <- x
    if(any(.isReplicated(x)))
       xj[.isReplicated(x)] <- jitter(x[.isReplicated(x)], factor=jit.fac)

    pp <- ppoints(n)
    yc <- q(y)(pp)

    yc.o <- yc

    if("support" %in% names(getSlots(class(y))))
       yc <- sort(jitter(yc, factor=jit.fac))

    col.pch <- rep(col.pch,length.out=n)
    col.lbl <- rep(col.lbl,length.out=n)

    ordx <- order(x)

    if(check.NotInSupport){
       xo <- x[ordx]
       nInSupp <- which(xo < q(y)(0))

       nInSupp <- unique(sort(c(nInSupp,which( xo > q(y)(1)))))
       if("support" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( ! xo %in% support(y)))))
       if("gaps" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( .inGaps(xo,gaps(y))))))
       if(length(nInSupp)){
          col.pch[nInSupp] <- col.NotInSupport
          if(withLab)
             col.lbl[ordx[nInSupp]] <- col.NotInSupport
       }
    }


    if(n!=length(x)) withLab <- FALSE

    mcl$x <- xj
    mcl$y <- yc
    mcl <- .deleteItemsMCL(mcl)
    mcl$cex <- cex.pch
    mcl$col <- col.pch


    ret <- do.call(stats::qqplot, args=mcl)

    if(withLab&& plot.it){
       lbprep <- .labelprep(xj,yc,lab.pts,
                            col.lbl,which.lbs,which.Order,order.traf)
       text(x = lbprep$x0, y = lbprep$y0, labels = lbprep$lab,
            cex = cex.lbl, col = lbprep$col, adj = adj.lbl)
    }

    if(withIdLine&& plot.it){
       abline(0,1,col=col.IdL,lty=lty.IdL,lwd=lwd.IdL)
       if(#is(y,"AbscontDistribution")&&
       withConf){
          xy <- unique(sort(c(x,yc.o)))
          lxy <- length(xy)
          if(lxy<n){
             xy0 <- seq(min(xy),max(xy),length=n-lxy+2)
             xy <- unique(sort(c(xy,xy0)))
          }
          .confqq(xy, y, alpha.CI, col.pCI, lty.pCI, lwd.pCI,
                  col.sCI, lty.sCI, lwd.sCI,
                  length(x), exact.sCI = exact.sCI, exact.pCI = exact.pCI)
       }
    }
    return(ret)
    })

## into distrMod
setMethod("qqplot", signature(x = "ANY",
                              y = "ProbFamily"), function(x, y,
                              n = length(x), withIdLine = TRUE, withConf = TRUE,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...){

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
    ylab = deparse(substitute(y)), ..., distance = NormType()){

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
    ylab = deparse(substitute(y)), ...){

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
