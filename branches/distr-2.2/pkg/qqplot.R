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
  if(is.null(gapm)) return(rep(FALSE,length(x)))
  fct <- function(x,m){ m[,2]>=x & m[,1]<=x}
  sapply(x, function(y) length(which(fct(y,gapm)))>0)
}

.isReplicated <- function(x){
  tx <- table(x)
  rx <- as.numeric(names(tx[tx>1]))
  sapply(x, function(y) any(abs(y-rx)<.Machine$double.eps))
}

.NotInSupport <- function(x,D){
  if(length(x)==0) return(logical(0))
  nInSupp <- which(x < q(D)(0))
  nInSupp <- unique(sort(c(nInSupp,which(x > q(D)(1)))))

  nInSuppo <-
      if("support" %in% names(getSlots(class(D))))
         which( ! x %in% support(D)) else numeric(0)
  if("gaps" %in% names(getSlots(class(D)))){
         InGap <- which( .inGaps(x,gaps(D)))
         if("support" %in% names(getSlots(class(D))))
            nInSupp <- unique(sort(c(nInSupp, intersect(InGap,nInSuppo))))
         else
            nInSupp <- unique(sort(c(nInSupp, InGap)))
  }else{
         nInSupp <- unique(sort(c(nInSupp, nInSuppo)))
  }
  return((1:length(x)) %in% nInSupp)
}

.isEqual <- distr:::.isEqual

.SingleDiscrete <- function(x,D){
  ## produces a logical vector of
  ##     0  : discrete mass point
  ##     1  : within continuous support
  ##     2  : left gap point
  ##     3  : right gap point
  ##     4  : not in support
  lx <- x * 0

  lx[.NotInSupport(x,D)] <- 4

  idx.0 <- ((x>q(D)(1)) | (x<q(D)(0)))
  iG <- rep(FALSE,length(x))

  if(is(D, "DiscreteDistribution")){
     return(lx)
  }
  if("gaps" %in% names(getSlots(class(D)))){
     if(!is.null(gaps(D))){
        lx[apply(sapply(gaps(D)[,1], function(u) .isEqual(u,x)),1,any)] <- 2
        lx[apply(sapply(gaps(D)[,2], function(u) .isEqual(u,x)),1,any)] <- 3
        iG <- .inGaps(x,gaps(D))
        lx[!idx.0 & !iG] <- 1
     }else{
        lx[!idx.0 & !iG] <- 1
     }
  }
  if("support" %in% names(getSlots(class(D)))){
     idx <- x %in% support(D)
     if("acPart" %in% names(getSlots(class(D))))
         idx.0 <- ((x>q.ac(D)(1)) | (x<q.ac(D)(0)))
     lx[idx & (idx.0|iG)] <- 0
  }

  return(lx)
}


.makeLenAndOrder <- function(x,ord){
   n <- length(ord)
   x <- rep(x, length.out=n)
   x[ord]
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

.BinomCI.in <- function(t,p.bi,x.i, del.i=0,D.i,n.i,alpha.i){
   p.bi.u <- p(D.i)(x.i+(t+del.i)/sqrt(n.i))
   p.bi.l <- p.l(D.i)(x.i-(t-del.i)/sqrt(n.i))
   d.r <- if(n.i*p.bi>floor(n.i*p.bi)) 0 else
        dbinom(x = n.i*p.bi, size = n.i, prob = pmax(p.bi.u,1))
   p.r <- pbinom(q = n.i*p.bi, size = n.i, prob = pmin(p.bi.u,1),lower.tail=FALSE)+d.r
   p.l <- pbinom(q = n.i*p.bi, size = n.i, prob = pmax(p.bi.l,0),lower.tail=FALSE)
   r <- p.r -p.l - alpha.i
#   print(c(r=r,p=p.bi,x=x.i,p.u=p.bi.u,p.l=p.bi.l,r.r=p.r,r.l=p.l,t=t,np=n*p.bi))
   r
  }


.BinomCI <- function(x,p.b,D,n,alpha){
  if(length(x)==0) return(NA)
  res <- sapply(1:length(x), function(i) uniroot(.BinomCI.in,
         lower=0, upper=sqrt(n)*max(x[i],n-x[i])+1,
         p.bi = p.b[i], x.i = x[i], del.i = 0,
         D.i = D, n.i = n, alpha.i = alpha, tol = 1e-9)$root)
  return(cbind(left=-res, right=res))
}

.BinomCI.nosym <- function(x,p.b,D,n,alpha){
  if(length(x)==0) return(NA)
  res0 <- sapply(1:length(x), function(i){
    get.t <- function(del.o, p.bi, x.i)
              uniroot(.BinomCI.in,
                lower=0, upper=sqrt(n)*max(x.i,n-x.i)+1,
                p.bi = p.bi, x.i = x.i, del.i=del.o,
                D.i = D, n.i = n, alpha.i = alpha, tol = 1e-9)$root
    res <- optimize(get.t, lower=-sqrt(n)*max(x[i],n-x[i])-1,
                    upper = sqrt(n)*max(x[i],n-x[i])+1, p.bi = p.b[i],
                    x = x[i], tol = 1e-9)
    t.o <- res$objective
    del <- res$minimum
    c(left=-t.o+del, right=t.o+del)
    })
   return(t(res0))
}


.q2pw <- function(x,p.b,D,n,alpha,exact=(n<100),nosym=FALSE){
 if(exact){
    fct <- if(nosym) .BinomCI.nosym else .BinomCI
    ro <- fct(x,p.b,D,n,alpha)
    return(ro)
 }
 pq <- log(p.b)+log(1-p.b)
 if(is(D,"AbscontDistribution")){
    dp <- d(D)(x,log=TRUE)
    dsupp.p <- dsupp.m<-1
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
    dp <- log(pnorm((x+dsupp.p/2-m)/s) - pnorm((x-dsupp.m/2-m)/s))
 }
 ro <- exp(pq/2-dp)*(dsupp.p+dsupp.m)/2*qnorm((1+alpha)/2)
 return(cbind(left=-ro,right=ro))
}


## to be exported: berechnet Konfidenzbänder, simultan und punktweise
qqbounds <- function(x,D,alpha,n,withConf.pw, withConf.sim,
                     exact.sCI=(n<100),exact.pCI=(n<100),
                     nosym.pCI = FALSE){
   x <- sort(unique(x))
   if("gaps" %in% names(getSlots(class(D))))
       {if(!is.null(gaps(D)))
            x <- sort(unique(c(x, gaps(D))))
       }
   c.c <- matrix(NA,nrow=length(x),ncol=4)
   colnames(c.c) <- c("sim.left","sim.right","pw.left","pw.right")

   SI <- .SingleDiscrete(x,D)
   SI.in <- SI<4
   SIi <- SI[SI.in]
   x.in <- x[SI.in]
   p.r <- p(D)(x.in)
   p.l <- p.l(D)(x.in)

   if(withConf.sim)
        c.crit <- try(.q2kolmogorov(alpha,n,exact.sCI), silent=TRUE)
   if(withConf.pw)
        c.crit.i <- try(
            .q2pw(x.in,p.r,D,n,alpha,exact.pCI,nosym.pCI),silent=TRUE)

   te.i <- withConf.pw  & !is(c.crit.i,"try-error")
   te.s <- withConf.sim & !is(c.crit,  "try-error")

   if(te.s){
      c.crit.r <- q.r(D)(pmax(1-p.r-c.crit/sqrt(n),
                         getdistrOption("DistrResolution")),lower.tail=FALSE)
      c.crit.l <- q(D)(pmax(p.l-c.crit/sqrt(n),
                       getdistrOption("DistrResolution")))
      c.crit.l[SIi == 2 | SIi == 3] <- NA
      c.crit.r[SIi == 2 | SIi == 3] <- NA
      c.c[SI.in,1:2] <- cbind(c.crit.l,c.crit.r)
   }
   if(te.i){
      print(c.crit.i)
      c.crit.i <- x.in + c.crit.i/sqrt(n)
      c.crit.i[SIi == 2 | SIi == 3] <- NA
      c.c[SI.in,3:4] <- c.crit.i
   }
   return(list(crit = c.c, err=c(sim=te.s,pw=te.i)))
}

.confqq <- function(x,D, withConf.pw  = TRUE,  withConf.sim = TRUE, alpha,
                    col.pCI, lty.pCI, lwd.pCI, pch.pCI, cex.pCI,
                    col.sCI, lty.sCI, lwd.sCI, pch.sCI, cex.sCI,
                    n,exact.sCI=(n<100),exact.pCI=(n<100), nosym.pCI = FALSE){

   x <- sort(unique(x))
   if("gaps" %in% names(getSlots(class(D))))
       {if(!is.null(gaps(D)))
            x <- sort(unique(c(x, gaps(D))))
       }
   SI <- .SingleDiscrete(x,D)
#   print(SI)
   SI.in <- SI<4
   SIi <- SI[SI.in]
   SI.c <- SIi>0
   x.in <- x[SI.in]
   x.c <- x.in[SI.c]
   x.d <- x.in[!SI.c]

   qqb <- qqbounds(x,D,alpha,n,withConf.pw, withConf.sim,
                   exact.sCI,exact.pCI,nosym.pCI)

   if(qqb$err["pw"]){
      if(sum(SI.c)>0){
         lines(x.c, qqb$crit[SI.c,"pw.right"],
            col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
         lines(x.c, qqb$crit[SI.c,"pw.left"],
            col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
      }
      if(sum(!SI.c)>0){
         points(x.d, qqb$crit[!SI.c,"pw.right"],
            col=col.pCI, pch=pch.pCI, cex = cex.pCI)
         points(x.d, qqb$crit[!SI.c,"pw.left"],
            col=col.pCI, pch=pch.pCI, cex = cex.pCI)
      }
   }
   if(qqb$err["sim"]){
      if(sum(SI.c)>0){
         lines(x.c, qqb$crit[SI.c,"sim.right"],
               col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
         lines(x.c, qqb$crit[SI.c,"sim.left"],
               col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
      }
      if(sum(!SI.c)>0){
         points(x.d, qqb$crit[!SI.c,"sim.right"],
                col=col.sCI, pch=pch.sCI, cex = cex.sCI)
         points(x.d, qqb$crit[!SI.c,"sim.left"],
                col=col.sCI, pch=pch.sCI, cex = cex.sCI)
      }
   }
   if( qqb$err["pw"] ||  qqb$err["sim"] ){
      expression1 <- substitute(
         nosym0~"pointw."~ex.p~alpha==alpha0~"%- conf. interval",
         list(ex.p = if(exact.pCI) "exact" else "asympt.",
              alpha0 = alpha*100,
              nosym0 = if(nosym.pCI&&exact.pCI) "shortest asymm." else "symm"))
      expression2 <- substitute(
         "simult."~ex.s~alpha==alpha0~"%- conf. interval",
         list(ex.s = if(exact.sCI) "exact" else "asympt.",
              alpha0 = alpha*100))
      if(!qqb$err["sim"]){
         expression3 <- expression1
         lty0 <- lty.pCI
         col0 <- col.pCI
      }
      if(!qqb$err["pw"]){
         expression3 <- expression2
         lty0 <- lty.sCI
         col0 <- col.sCI
      }
      if( qqb$err["pw"] && qqb$err["sim"]){
         expression3 <- eval(substitute(expression(expression1, expression2)))
         lty0 <- c(lty.pCI, lty.sCI)
         col0 <- c(col.pCI,col.sCI)
      }
      legend("topleft", legend = expression3, bg = "white",
              lty = lty0, col = col0, lwd = 2, cex = .8)
   }
  return(invisible(NULL))
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
    mcl$exp.cex2.pch <- mcl$exp.cex2.lbl <- NULL
    mcl$exp.fadcol.pch <- mcl$exp.fadcol.lbl <- NULL
    mcl$nosym.pCI <- NULL
mcl}

## helper into distrMod
.labelprep <- function(x,y,lab.pts,col.lbl,cex.lbl,which.lbs,which.Order,order.traf){
      n <- length(x)
      rx <- rank(x)
      xys <- cbind(x,y[rx])
      if(is.null(which.lbs)) which.lbs <- 1:n
      oN0 <- order(x,decreasing=TRUE)
      if(!is.null(order.traf)){
          oN0 <- order(order.traf(x),decreasing=TRUE)
      }
      oN0b <- oN0 %in% which.lbs
      oN0 <- oN0[oN0b]
      oN <- oN0
      if(!is.null(which.Order))
          oN <- oN0[which.Order]
      x0 <- xys[oN,1]
      y0 <- xys[oN,2]

      col.lbl <- col.lbl[rx]
      lab.pts <- lab.pts[rx]
      cex.lbl <- cex.lbl[rx]
      return(list(x0=x0,y0=y0,lab=lab.pts[oN],col=col.lbl[oN],cex=cex.lbl[oN]))
}

#.makeLenAndOrder <- distr:::.makeLenAndOrder

.fadeColor <- function(col,x, bg = "white"){
 ind <- seq(along=x)
 col <- .makeLenAndOrder(col,ind)
 colx <- t(sapply(ind,function(i) colorRamp(c(bg,col[i]))(x[i])))
 colv2col <- function(colvec)
   rgb(red = colvec[1], green = colvec[2], blue = colvec[3], maxColorValue = 255)
 apply(colx,1,function(x) colv2col(x))
}

## into distr:
setMethod("qqplot", signature(x = "UnivariateDistribution",
                              y = "UnivariateDistribution"), function(x, y,
                              n = 30, withIdLine = TRUE, withConf = TRUE,
    withConf.pw  = withConf,  withConf.sim = withConf,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    col.IdL = "red", lty.IdL = 2, lwd.IdL = 2,
    alpha.CI = .95, exact.pCI = (n<100), exact.sCI = (n<100), nosym.pCI = FALSE,
    col.pCI = "orange", lty.pCI = 3, lwd.pCI = 2, pch.pCI = par("pch"), cex.pCI = par("cex"),
    col.sCI = "tomato2", lty.sCI = 4, lwd.sCI = 2, pch.sCI = par("pch"), cex.sCI = par("cex"),
    cex.pch = par("cex"), col.pch = par("col"),
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
       nInSupp <- .NotInSupport(xc,y)
       if(length(nInSupp)){
          col.pch[nInSupp] <- col.NotInSupport
       }
    }


    oxc <- 1:length(xc)
    xc.o <- xc
    yc.o <- yc
    ord.x <- order(xc)

    if("support" %in% names(getSlots(class(x)))){
       xc <- jitter(xc, factor=jit.fac)
       oxc <- order(xc)
       xc <- xc[oxc]
       }

    if("support" %in% names(getSlots(class(y))))
       yc <- sort(jitter(yc, factor=jit.fac))

    mcl$x <- xc
    mcl$y <- yc

    mcl <- .deleteItemsMCL(mcl)

    mcl$cex <- .makeLenAndOrder(cex.pch,ord.x)
    mcl$col <- .makeLenAndOrder(col.pch,ord.x)

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
          .confqq(xy, y, withConf.pw, withConf.sim, alpha.CI,
                      col.pCI, lty.pCI, lwd.pCI, pch.pCI, cex.pCI,
                      col.sCI, lty.sCI, lwd.sCI, pch.sCI, cex.sCI,
                  length(xc), exact.sCI = exact.sCI, exact.pCI = exact.pCI,
                  nosym.pCI = nosym.pCI)
       }
    }
    return(ret)
    })
    
## into distrMod
#.confqq <- distr:::.confqq
setMethod("qqplot", signature(x = "ANY",
                              y = "UnivariateDistribution"),
    function(x,    ### observations
             y,    ### distribution
             n = length(x), ### number of points to be plotted
             withIdLine = TRUE, ### shall line y=x be plotted in
             withConf = TRUE,   ### shall confidence lines be plotted
             withConf.pw  = withConf,   ### shall pointwise confidence lines be plotted
             withConf.sim = withConf,   ### shall simultaneous confidence lines be plotted
             plot.it = TRUE,    ### shall be plotted at all (inherited from stats::qqplot)
             xlab = deparse(substitute(x)), ## x-label
             ylab = deparse(substitute(y)), ## y-label
             ...,                 ## further parameters
             withLab = FALSE,     ## shall observation labels be plotted in
             lab.pts = NULL,      ## observation labels to be used
             which.lbs = NULL,    ## which observations shall be labelled
             which.Order = NULL,  ## which of the ordered (remaining) observations shall be labelled
             order.traf = NULL,   ## an optional trafo; by which the observations are ordered (as order(trafo(obs))
             col.IdL = "red",     ## color for the identity line
             lty.IdL = 2,         ## line type for the identity line
             lwd.IdL = 2,         ## line width for the identity line
             alpha.CI = .95,      ## confidence level
             exact.pCI = (n<100), ## shall pointwise CIs be determined with exact Binomial distribution?
             exact.sCI = (n<100), ## shall simultaneous CIs be determined with exact kolmogorov distribution?
             nosym.pCI = FALSE,   ## shall we use (shortest) asymmetric CIs?
             col.pCI = "orange",  ## color for the pointwise CI
             lty.pCI = 3,         ## line type for the pointwise CI
             lwd.pCI = 2,         ## line width for the pointwise CI
             pch.pCI = par("pch"),## symbol for points (for discrete mass points) in pointwise CI
             cex.pCI = par("cex"),## magnification factor for points (for discrete mass points) in pointwise CI
             col.sCI = "tomato2", ## color for the simultaneous CI
             lty.sCI = 4,         ## line type for the simultaneous CI
             lwd.sCI = 2,         ## line width for the simultaneous CI
             pch.sCI = par("pch"),## symbol for points (for discrete mass points) in simultaneous CI
             cex.sCI = par("cex"),## magnification factor for points (for discrete mass points) in simultaneous CI
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

    ord.x <- order(xj)

    pp <- ppoints(n)
    yc <- q(y)(pp)

    yc.o <- yc

    if("support" %in% names(getSlots(class(y))))
       yc <- sort(jitter(yc, factor=jit.fac))

    cex.pch <- .makeLenAndOrder(cex.pch,ord.x)
    cex.lbl <- .makeLenAndOrder(cex.lbl,ord.x)
    col.pch <- .makeLenAndOrder(col.pch,ord.x)
    col.lbl <- .makeLenAndOrder(col.lbl,ord.x)

    if(withLab){
      if(is.null(lab.pts)) lab.pts <- paste(ord.x)
      else lab.pts <- .makeLenAndOrder(lab.pts,ord.x)
    }

    if(check.NotInSupport){
       xo <- x[ord.x]
       nInSupp <- which(xo < q(y)(0))

       nInSupp <- unique(sort(c(nInSupp,which( xo > q(y)(1)))))
       if("support" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( ! xo %in% support(y)))))
       if("gaps" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( .inGaps(xo,gaps(y))))))
       if(length(nInSupp)){
          col.pch[nInSupp] <- col.NotInSupport
          if(withLab)
#             col.lbl[ord.x[nInSupp]] <- col.NotInSupport
             col.lbl[nInSupp] <- col.NotInSupport
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
                            col.lbl,cex.lbl,which.lbs,which.Order,order.traf)
       text(x = lbprep$x0, y = lbprep$y0, labels = lbprep$lab,
            cex = lbprep$cex, col = lbprep$col, adj = adj.lbl)
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
          .confqq(xy, y, withConf.pw, withConf.sim, alpha.CI,
                      col.pCI, lty.pCI, lwd.pCI, pch.pCI, cex.pCI,
                      col.sCI, lty.sCI, lwd.sCI, pch.sCI, cex.sCI,
                  length(x), exact.sCI = exact.sCI, exact.pCI = exact.pCI,
                  nosym.pCI = nosym.pCI)
       }
    }
    return(ret)
    })

## into distrMod
setMethod("qqplot", signature(x = "ANY",
                              y = "ProbFamily"), function(x, y,
                              n = length(x), withIdLine = TRUE, withConf = TRUE,
    withConf.pw  = withConf,  withConf.sim = withConf,
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
    withConf.pw  = withConf,  withConf.sim = withConf,
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
             withConf.pw  = withConf,   ### shall pointwise confidence lines be plotted
             withConf.sim = withConf,   ### shall simultaneous confidence lines be plotted
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]
    if(is.null(mcl$distance)) distance <- NormType()

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

## into RobAStBase
setMethod("qqplot", signature(x = "ANY",
                              y = "kStepEstimate"), function(x, y,
                              n = length(x), withIdLine = TRUE, withConf = TRUE,
    withConf.pw  = withConf,  withConf.sim = withConf,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...,
    exp.cex2.lbl = -.15,
    exp.cex2.pch = -.35,
    exp.fadcol.lbl = 1.85,
    exp.fadcol.pch = 1.85,
    bg = "white"
    ){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    IC <- pIC(y)
    if(!is(IC,"IC"))
       stop("IC of the kStepEstimator needs to be of class 'IC'")
    
    L2Fam <- eval(IC@CallL2Fam)

    mcl$y <- L2Fam

    if(is(IC,"HampIC")){
      w.fct <- weight(weight(IC))
      wx <- sapply(x,w.fct)
      mcl$order.traf <- function(x) 1/w.fct(x)

      cex.lbl <- if(is.null(mcl$cex.lbl))  par("cex")  else eval(mcl$cex.lbl)
      cex.pch <- if(is.null(mcl$cex.pch))  par("cex")  else eval(mcl$cex.pch)
      mcl$cex.lbl <- cex.lbl*wx^exp.cex2.lbl
      mcl$cex.pch <- cex.pch*wx^exp.cex2.pch

      col.lbl <- if(is.null(mcl$col.lbl))  par("col")  else eval(mcl$col.lbl)
      col.pch <- if(is.null(mcl$col.pch))  par("col")  else eval(mcl$col.pch)
      mcl$col.lbl <- .fadeColor(col.lbl,wx^exp.fadcol.lbl, bg = bg)
      mcl$col.pch <- .fadeColor(col.pch,wx^exp.fadcol.pch, bg = bg)
    }

    print(mcl)
    return(do.call(getMethod("qqplot", signature(x="ANY", y="ProbFamily")),
            args=mcl))
    })
