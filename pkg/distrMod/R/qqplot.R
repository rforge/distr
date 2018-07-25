################################################################
# QQ - Plot functions in package distrMod
################################################################

### to be written into the respective MASKING files....


## helper into distrMod
.labelprep <- function(x,y,lab.pts,col.lbs,cex.lbs,adj.lbs,which.lbs,which.Order,order.traf, which.nonlbs){
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
      
      col.lbs <- col.lbs[rx]
      lab.pts <- lab.pts[rx]
      cex.lbs <- cex.lbs[rx]
      adj.lbs <- adj.lbs[rx]
      ind <- 1:n
      ind.ns <- ind[-oN]
      if(!is.null(which.nonlbs)) ind.ns <- ind.ns[ind.ns %in% which.nonlbs]
      return(list(x0=x0,y0=y0,lab=lab.pts[oN],col=col.lbs[oN],cex=cex.lbs[oN],adj=adj.lbs[oN], ord=oN, ns=ind.ns))
}




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
             datax = FALSE,     ### as in qqnorm
             xlab = deparse(substitute(x)), ## x-label
             ylab = deparse(substitute(y)), ## y-label
             ...,                 ## further parameters
             width = 10,          ## width (in inches) of the graphics device opened
             height = 5.5,        ## height (in inches) of the graphics device opened}
             withSweave = getdistrOption("withSweave"), ## logical: if \code{TRUE}
             ##               (for working with \command{Sweave}) no extra device is opened and height/width are not set
             mfColRow = TRUE,     ## shall we use panel partition mfrow=c(1,1)?
             n.CI = n,            ## number of points to be used for CI
             withLab = FALSE,     ## shall observation labels be plotted in
             lab.pts = NULL,      ## observation labels to be used
             which.lbs = NULL,    ## which observations shall be labelled
             which.Order = NULL,  ## which of the ordered (remaining) observations shall be labelled
             which.nonlbs = NULL, ## which of the non-labelled observations shall be plotted
             attr.pre = FALSE,    ## do indices refer to order pre or post ordering
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
             added.points.CI = TRUE, ## should the CIs be drawn through additional points?
             cex.pch = par("cex"),## magnification factor for the plotted symbols (for backward compatibility only, cex.pts in the sequel)
             col.pch = par("col"),## color for the plotted symbols (for backward compatibility only, col.pts in the sequel)
             cex.pts = 1,         ## magnification factor for labelled shown observations
             col.pts = par("col"),## color for labelled shown observations
             pch.pts = 19,        ## symbol for labelled shown observations
             cex.npts = 1,        ## magnification factor for non-labelled shown observations
             col.npts = grey(.5), ## color for non-labelled shown observations
             pch.npts = 20,       ## symbol for non-labelled shown observations
             cex.lbs = par("cex"),## magnification factor for the plotted observation labels
             col.lbs = par("col"),## color for the plotted observation labels
             adj.lbs = par("adj"),## adj parameter for the plotted observation labels
             alpha.trsp = NA,     ## alpha transparency to be added afterwards
             jit.fac = 0,         ## jittering factor used for discrete distributions
             jit.tol = .Machine$double.eps, ## tolerance for jittering: if distance 
                                 #is smaller than jit.tol, points are considered replicates
             check.NotInSupport = TRUE, ## shall we check if all x lie in support(y)
             col.NotInSupport = "red", ## if preceding check TRUE color of x if not in support(y)
             with.legend = TRUE,  ## shall a legend be plotted
             legend.bg = "white", ## background for the legend
             legend.pos = "topleft", ## position for the legend
             legend.cex = 0.8,     ## magnification factor for the legend
             legend.pref = "",     ## prefix for legend  text
             legend.postf = "",    ## postfix for legend text
             legend.alpha = alpha.CI, ## nominal level of CI
             debug = FALSE, ## shall additional debug output be printed out?
             withSubst = TRUE
    ){ ## return value as in stats::qqplot

    mc <- match.call(call = sys.call(sys.parent(1)))
    xcc <- as.character(deparse(mc$x))
    ycc <- as.character(deparse(mc$y))
    if(missing(xlab)){ xlab <- mc$xlab <- xcc}
    if(missing(ylab)){ ylab <- mc$ylab <- ycc}
    mcl <- as.list(mc)[-1]
    force(x)
    if(is.null(mcl$datax)) datax <- FALSE
    if(!datax){ mcl$ylab <- xlab; mcl$xlab <- ylab}

   .mpresubs <- if(withSubst){
                   function(inx) 
                    .presubs(inx, c("%C", "%A", "%D" ),
                          c(as.character(class(x)[1]), 
                            as.character(date()), 
                            xcc))
               }else function(inx)inx
    xj <- x
    if(any(.isReplicated(x, jit.tol))&&jit.fac>0)
       xj[.isReplicated(x, jit.tol)] <- jitter(x[.isReplicated(x, jit.tol)], factor=jit.fac)

    ord.x <- order(xj)

    pp <- ppoints(n)
    yc <- q.l(y)(pp)

    yc.o <- yc

    if("support" %in% names(getSlots(class(y))))
       yc <- sort(jitter(yc, factor=jit.fac))

    alp.v <- .makeLenAndOrder(alpha.trsp,ord.x)
    alp.t <- function(x,a1) if(is.na(x)) x else addAlphTrsp2col(x,a1)
    alp.f <- if(length(alpha.trsp)==1L && is.na(alpha.trsp))
             function(x,a) x else function(x,a) mapply(x,alp.t,a1=a)
    cex.lbs <- .makeLenAndOrder(cex.lbs,ord.x)
    adj.lbs <- .makeLenAndOrder(adj.lbs,ord.x)
    col.lbs <- alp.f(.makeLenAndOrder(col.lbs,ord.x),alp.v)

    lbprep <- .labelprep(x = xj, y = yc.o, lab.pts = lab.pts,
                         col.lbs = col.lbs, cex.lbs = cex.lbs,
                         adj.lbs = adj.lbs, which.lbs = which.lbs,
                         which.Order = which.Order, order.traf = order.traf,
                         which.nonlbs = which.nonlbs)
    n.ns <- length(lbprep$ns)
    n.s <- length(lbprep$ord)

    shown <- c(lbprep$ord,lbprep$ns)

    if(attr.pre){
       cex.pch <- .makeLenAndOrder(cex.pch,ord.x)
       col.pch <- alp.f(.makeLenAndOrder(col.pch,ord.x),alp.v)
       cex.pts <- if(missing(cex.pts)) cex.pch else .makeLenAndOrder(cex.pts,ord.x)
       col.pts <- if(missing(col.pts)) col.pch else alp.f(.makeLenAndOrder(col.pts,ord.x),alp.v)
       pch.pts <- .makeLenAndOrder(pch.pts,ord.x)
       cex.pts <- cex.pts[shown]
       col.pts <- col.pts[shown]
       pch.pts <- pch.pts[shown]
    }else{
       cex.pch <- rep(cex.pch,length.out=n.s)
       col.pch <- alp.f(rep(col.pch,length.out=n.s),alp.v)
       cex.pts <- if(missing(cex.pts)) cex.pch else rep(cex.pts,length.out=n.s)
       col.pts <- if(missing(col.pts)) col.pch else alp.f(rep(cex.pts,length.out=n.s),alp.v[lbprep$ord])
       pch.pts <- rep(pch.pts,length.out=n.s)
       cex.npts <- rep(cex.pts,length.out=n.ns)
       col.npts <- alp.f(rep(cex.pts,length.out=n.ns),alp.v[lbprep$ns])
       pch.npts <- rep(pch.pts,length.out=n.ns)
       col.pts <- c(col.pts,col.npts)
       cex.pts <- c(cex.pts,cex.npts)
       pch.pts <- c(pch.pts,pch.npts)
    }
    xs <- x[shown]
    ycs <- yc.o[shown]
    ordx <- order(xs)
    xso <- xs[ordx]
    ycso <- ycs[ordx]
    cex.pts <- cex.pts[ordx]
    col.pts <- col.pts[ordx]
    pch.pts <- pch.pts[ordx]

    if(withLab){
      if(is.null(lab.pts)) lab.pts <- paste(ord.x)
      else lab.pts <- .makeLenAndOrder(lab.pts,ord.x)
    }

    if(check.NotInSupport){
       xo <- xso #x[ord.x]
       nInSupp <- which(xo < q.l(y)(0))

       nInSupp <- unique(sort(c(nInSupp,which( xo > q.l(y)(1)))))
       if("support" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( ! xo %in% support(y)))))
       if("gaps" %in% names(getSlots(class(y))))
          nInSupp <- unique(sort(c(nInSupp,which( .inGaps(xo,gaps(y))))))
       if(length(nInSupp)){
#          col.pch[nInSupp] <- col.NotInSupport
          col.pts[nInSupp] <- col.NotInSupport
          if(withLab)
#             col.lbs[ord.x[nInSupp]] <- col.NotInSupport
             col.lbs[nInSupp] <- col.NotInSupport
       }
    }


    if(n!=length(x)) withLab <- FALSE

    if(datax){ 
      mcl$x <- xso#xj
      mcl$y <- ycso #yc
    }else{
      mcl$y <- xso# xj
      mcl$x <- ycso #yc
    }
    mcl <- .deleteItemsMCL(mcl)
    mcl$pch <- pch.pts
    mcl$cex <- cex.pts
    mcl$col <- col.pts

    mcl$xlab <- .mpresubs(mcl$xlab)
    mcl$ylab <- .mpresubs(mcl$ylab)


    if (!is.null(eval(mcl$main)))
        mcl$main <- .mpresubs(eval(mcl$main))
    if (!is.null(eval(mcl$sub)))
        mcl$sub <- .mpresubs(eval(mcl$sub))

    if (!withSweave){
           devNew(width = width, height = height)
    }
    opar <- par("mfrow", no.readonly = TRUE)
    if(mfColRow) on.exit(do.call(par, list(mfrow=opar, no.readonly = TRUE)))

    if(mfColRow) opar1 <- par(mfrow = c(1,1), no.readonly = TRUE)

    ret <- do.call(stats::qqplot, args=mcl)

    if(withLab&& plot.it){
       xlb0 <- if(datax) lbprep$x0 else lbprep$y0
       ylb0 <- if(datax) lbprep$y0 else lbprep$x0
       text(x = xlb0, y = ylb0, labels = lbprep$lab,
            cex = lbprep$cex, col = lbprep$col, adj = lbprep$adj)
    }

    qqb <- NULL
    if(withIdLine){
       if(plot.it) abline(0,1,col=col.IdL,lty=lty.IdL,lwd=lwd.IdL)
       if(#is(y,"AbscontDistribution")&&
       withConf){
          xy <- unique(sort(c(x,yc.o)))
          if(added.points.CI){
             mxy <- min(xy); Mxy <- max(xy)
             mnxy <- (mxy+Mxy)/2
             sxy <- (Mxy-mxy)/2*1.1
             xyn <- seq(mnxy-sxy,mnxy+sxy,length.out=500)
             xy <- unique(sort(c(xy,xyn)))
          }
          xy <- xy[!.NotInSupport(xy,y)]
          lxy <- length(xy)
          if(is(y,"DiscreteDistribution")){
             n0 <- min(n.CI, length(support(y)))
             n1 <- max(n0-lxy,0)
             if (n1 >0 ){
                 notyetInXY <- setdiff(support(y), xy)
                 xy0 <- sample(notyetInXY, n1)
                 xy <- sort(unique(c(xy,xy0)))
             }
          }else{
             if(lxy < n.CI){
                n1 <- (n.CI-lxy)%/%3
                xy0 <- seq(min(xy),max(xy),length=n1)
                xy1 <- r(y)(n.CI-lxy-n1)
                xy <- sort(unique(c(xy,xy0,xy1)))
             }
          }

        qqplotInfo <- list(xy.0=xy, y.0=y, datax = datax, 
                         withConf.pw=withConf.pw, 
                         withConf.sim=withConf.sim, 
                         alpha.CI=alpha.CI ,
                         col.pCI = col.pCI , lty.pCI = lty.pCI , 
                         lwd.pCI = lwd.pCI , pch.pCI = pch.pCI, 
                         cex.pCI = cex.pCI , 
                         col.sCI = col.sCI , lty.sCI = lty.sCI , 
                         lwd.sCI = lwd.sCI , pch.sCI = pch.sCI, 
                         cex.sCI = cex.sCI , 
                         n = n , 
                         exact.sCI = exact.sCI, exact.pCI = exact.pCI,
                  nosym.pCI = nosym.pCI, with.legend = with.legend,
                  legend.bg = legend.bg, legend.pos = legend.pos,
                  legend.cex = legend.cex, legend.pref = legend.pref,
                  legend.postf = legend.postf, legend.alpha = legend.alpha, 
                  debug = debug,
                  args.stats.qqplot = mcl,
                  withLab = withLab,
                  lbprep = lbprep
                  )
        if(plot.it){
          qqb <- .confqq(xy, y, datax=datax, withConf.pw, withConf.sim, alpha.CI,
                      col.pCI, lty.pCI, lwd.pCI, pch.pCI, cex.pCI,
                      col.sCI, lty.sCI, lwd.sCI, pch.sCI, cex.sCI,
                  n, exact.sCI = exact.sCI, exact.pCI = exact.pCI,
                  nosym.pCI = nosym.pCI, with.legend = with.legend,
                  legend.bg = legend.bg, legend.pos = legend.pos,
                  legend.cex = legend.cex, legend.pref = legend.pref,
                  legend.postf = legend.postf, legend.alpha = legend.alpha, debug = debug)
        }else{
           qqb <- qqbounds(sort(unique(xy)),y,alpha.CI,n,withConf.pw, withConf.sim,
                           exact.sCI,exact.pCI,nosym.pCI, debug = debug)
        }
       }
    }
    qqplotInfo <- c(call=mc, ret, qqplotInfo, qqb)
    class(qqplotInfo) <- c("qqplotInfo","DiagnInfo")
    return(invisible(qqplotInfo))
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

    retv <- do.call(getMethod("qqplot", signature(x="ANY", y="UnivariateDistribution")),
            args=mcl)
    retv$call <- mc        
    return(invisible(retv))
    })

setMethod("qqplot", signature(x = "ANY",
                              y = "Estimate"), function(x, y,
                              n = length(x), withIdLine = TRUE, withConf = TRUE,
    withConf.pw  = withConf,  withConf.sim = withConf,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...){

    mc <- match.call(call = sys.call(sys.parent(1)))
    if(missing(xlab)) mc$xlab <- as.character(deparse(mc$x))
    if(missing(ylab)) mc$ylab <- as.character(deparse(mc$y))
    mcl <- as.list(mc)[-1]

    param <- ParamFamParameter(main=untransformed.estimate(y), nuisance=nuisance(y),
                               fixed=fixed(y))

    es.call <- y@estimate.call
    nm.call <- names(es.call)
    PFam <- NULL
    if("ParamFamily" %in% nm.call)
       PFam <- eval(as.list(es.call)[["ParamFamily"]])
    if(is.null(PFam))
       stop("There is no object of class 'ProbFamily' in the call of 'x'")

    PFam0 <- modifyModel(PFam, param)
    mcl$y <- PFam0
    retv <- do.call(getMethod("qqplot", signature(x="ANY", y="ProbFamily")),
            args=mcl)
    retv$call <- mc        
    return(invisible(retv))
    })

