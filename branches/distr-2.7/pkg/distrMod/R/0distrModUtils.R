.isUnitMatrix <- function(m){
### checks whether m is unit matrix
              m.row <- nrow(m)
              isTRUE(all.equal(m, diag(m.row), check.attributes = FALSE))
              }

.deleteDim <- function(x){
     attribs <- attributes(x)
     attribs$dim <- NULL
     attribs$dimnames <- NULL
     attributes(x) <- attribs
     x
     }

.getLogDeriv <- function(distr,
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
                         IQR.fac = getdistrExOption("IQR.fac")){
  low0 <- q.l(distr)(lowerTruncQuantile)
  upp0 <- q.l(distr)(upperTruncQuantile,lower.tail=FALSE)
  me <- median(distr)
  s1 <- IQR(distr)
  low1 <- me - IQR.fac * s1 
  upp1 <- me + IQR.fac * s1 
  low <- max(low0,low1); upp <- min(upp0, upp1)
  xs <- seq(low, upp, length = getdistrOption("DefaultNrGridPoints"))
  m <- getdistrOption("DefaultNrGridPoints")%/%100+1
  dxs<- -d(distr)(xs, log = TRUE)
#  plot(xs, dxs,type="l")
  x1 <- xs[1]; xn <- (rev(xs)[1])
  f2xs <- approxfun(x = xs, y = D2ss(xs,dxs)$y, rule = 2)
  f2x1 <- f2xs(x1); f2xn <- f2xs(xn);
  f1xs <- approxfun(x = xs, y = D1ss(xs,dxs))
  f1x1 <- f1xs(x1); f1xn <- f1xs(xn);
  f3xs <- approxfun(x = xs, y = D2ss(xs,f1xs(xs))$y, rule = 1)
  f3x1 <- median(f3xs(xs[1:m])); f3xn <- median(f3xs(rev(xs)[1:m]));
#  windows()
#  plot(xs, f1xs(xs),type="l")
#  print(xn); print(x0); print(f3x1); print(f3xn); print(f1x1); print(f1xn); print(f2x1); print(f2xn);
  fxs <- function(x){
       f1x0 <- f1xs(x)
       dx1 <- (x[x<x1]-x1)
       dxn <- (x[x>xn]-xn)
       f1x0[x>xn] <- f1xn + f2xn*dxn + f3xn/2*dxn^2
       f1x0[x<x1] <- f1x1 + f2x1*dx1 + f3x1/2*dx1^2
       return(f1x0)}
  return(fxs)
}

.show.with.sd <- function(est, s){
#        est <- as.numeric(est); dim(est) <- NULL
#        s <- as.numeric(s); dim(s) <- NULL
        if(is.null(names(est))) names(est) <- rep("", length.out=length(est))
  ### code borrowed from print.fitdistr in  package MASS by B.D. Ripley
        digits <- getOption("digits")
        ans <- format(base::rbind(est, s), digits=digits)
        ans[1L, ] <- sapply(ans[1L, ], function(x) paste("", x))
        ans[2L, ] <- sapply(ans[2L, ], function(x) paste("(", x, ")", sep=""))
     ## only used for digits
        dn <- dimnames(ans)
        dn[[1L]] <- rep("", 2L)
        dn[[2L]] <- paste(substring("      ", 1L,
                       (nchar(ans[2L,]) - nchar(dn[[2L]])) %/% 2), dn[[2L]])
        dn[[2L]] <- paste(dn[[2L]], substring("      ", 1L,
                       (nchar(ans[2L,]) - nchar(dn[[2L]])) %/% 2))
        dimnames(ans) <- dn
        print(ans, quote = FALSE)
        return(invisible())
        }
 ### end of borrowed code  


.validTrafo <- function(trafo, dimension, dimensionwithN){
##checks whether trafo is valid
  ret <- FALSE
  if(!is.function(trafo)){
    if((ncol(trafo) != dimension) && (ncol(trafo) != dimensionwithN))
        stop("invalid transformation:\n", 
             "number of columns of 'trafo' not equal to ", 
             "dimension of the parameter")
#    if(nrow(trafo) > dimension)
#        stop("invalid transformation:\n",
#             "number of rows of 'trafo' larger than ",
#             "dimension of the parameter")
    if(any(!is.finite(trafo)))
        stop("infinite or missing values in 'trafo'")
    ret <- (ncol(trafo) == dimensionwithN)
    }
  return(ret)
}


.CvMMDCovariance<- function(L2Fam, param, mu = distribution(L2Fam),  
                            withplot = FALSE, withpreIC = FALSE,
                            N = getdistrOption("DefaultNrGridPoints")+1,
                            rel.tol=.Machine$double.eps^0.3, 
                            TruncQuantile = getdistrOption("TruncQuantile"), 
                            IQR.fac = 15, 
                            ...){

   # preparations:

   N1 <- 2*N+1
   odd <- (1:N1)%%2==1

   param0 <- L2Fam@param
   dim0 <- dimension(param0)
#   print(param0)
   paramP <- param0
   paramP@main <- main(param)
   paramP@trafo <- diag(dim0)
#   print(paramP)
   L2Fam <- modifyModel(L2Fam, paramP)

#   print(L2deriv(L2Fam)[[1]]@Map)
   distr <- L2Fam@distribution
   
   ### get a sensible integration range:
   low0 <- q.l(distr)(TruncQuantile)
   up0 <- q.l(distr)(TruncQuantile, lower.tail = FALSE)
   m0 <- median(distr); s0 <- IQR(distr)
   low1 <- m0 - IQR.fac * s0
   up1  <- m0 + IQR.fac * s0
   low <- max(low0,low1); up <- min(up0,up1)

   ### get a sensible integration range:
   if(missing(mu)) mu <- distr
   low0.mu <- q.l(mu)(TruncQuantile)
   up0.mu <- q.l(mu)(TruncQuantile, lower.tail = FALSE)
   m0.mu <- median(mu); s0.mu <- IQR(mu)
   low1.mu <- m0.mu - IQR.fac * s0.mu
   up1.mu  <- m0.mu + IQR.fac * s0.mu
   low.mu <- max(low0.mu,low1.mu); up.mu <- min(up0.mu,up1.mu)


   if(is(distr,"DiscreteDistribution"))
       x.seq <-support(distr)
   else
       {if(is(distr,"AbscontDistribution")){
           x.seq0 <- seq(low, up, length = N1)
           h0 <- diff(x.seq0[2:1])
           x.seq <- x.seq0[odd]
          }else{ 
           x.seq <- seq(low,up, length = N)
          }
       }
   if(is(mu,"DiscreteDistribution"))
       x.mu.seq <- support(mu)
   else
       {if(is(mu,"AbscontDistribution")){
           x.mu.seq0 <- seq(low.mu, up.mu, length = N1)
           h0.mu <- diff(x.mu.seq0[2:1])
           x.mu.seq <- x.mu.seq0[odd]
          }else{ 
           x.mu.seq <- seq(low.mu, up.mu, length = N)
          }
       }
   
   L2deriv <- L2deriv(L2Fam)[[1]]
#   y.seq <- sapply(x.seq, function(x) evalRandVar(L2deriv, x))
#   plot(x.seq[!is.na(y.seq)],y.seq ,type="l")

   ## are we working with a one-dim L2deriv or not?

   onedim <- (length(L2deriv@Map)==1)


   if(onedim){
   ## one-dim case

   ## Delta, formula (56), p. 133 [Ri:94]
   ##        Ptheta- primitive function for Lambda

   if(is(distr,"AbscontDistribution")){
      Delta0x <- sapply(x.seq0, function(x) 
                                evalRandVar(L2deriv, x)) * 
                 d(distr)(x.seq0)
      Delta0 <-  h0*.csimpsum(Delta0x)   
   }else{
      L2x  <- function(x,y)  (x<=y)*evalRandVar(L2deriv, x)
      Delta0 <- sapply(x.seq, function(Y){ fct <- function(x) L2x(x,y=Y)
                                        return(E(object=distr, fun = fct))})
   }
 #  print(Delta0)
   Delta1 <- approxfun(x.seq, Delta0, yleft = 0, yright = 0)
   if(is(distr,"DiscreteDistribution"))         
      Delta <- function(x) Delta1(x) * (x %in% support(distr))
   else  Delta <- function(x) Delta1(x)
 #  print(Delta(x.seq))
 #  print(Delta(rnorm(100)))

   ## J = Var_Ptheta Delta
   J1 <- E(object=distr, fun = Delta)
#   print(J1)
   Delta.0 <- function(x) Delta(x) - J1
 #  print(Delta.0(x.seq))
 #  print(Delta.0(r(distr)(100))^2)
   #J <- distrExIntegrate(function(x) d(distr)(x)*Delta.0(x)^2, lower=low, upper=up)
   J <- E(object=distr, fun = function(x) Delta.0(x)^2 )
#   print(J)
   
   ### CvM-IC phi
   phi <- function(x) Delta.0(x)/J

   ## integrand phi x Ptheta in formula (51) [ibid]
   phi1 <- function(x) phi(x) * p(distr)(x)
   psi1 <- E(object = mu, fun = phi1)


   ## obtaining IC psi  (formula (51))

   if(is(mu,"AbscontDistribution")){
      phix <- function(x) phi(x)*d(mu)(x)
      psi0x <- sapply(rev(x.mu.seq0), phix)
      psi0 <-  h0.mu*rev(.csimpsum(psi0x))   
   }else{
      phixy  <- function(x,y)  (x<=y)*phi(y)
      psi0 <- sapply(x.mu.seq, function(X){ fct <- function(y) phixy(x=X,y=y)
                                        return(E(object=mu, fun = fct))})
   }
 #  print(psi0)
   psi.1 <- approxfun(x.mu.seq, psi0, yleft = 0, yright = rev(psi0)[1])
   if(is(distr,"DiscreteDistribution"))
         psi <- function(x) (psi.1(x)-psi1) * (x %in% support(mu))
   else  psi <- function(x) psi.1(x)-psi1

   E2 <- E(object=distr, fun = function(x) psi(x)^2)
   L2deriv <- L2Fam@L2deriv[[1]]
   ## E2 = Cov_mu (psi)

#   ### control: centering & standardization
   E1 <- E(object=distr, fun = psi )
   E3 <- E(object=distr, fun = function(x) psi(x)*evalRandVar(L2deriv, x))
   psi.0 <- function(x) psi(x) - E1
   psi.01 <- function(x) psi.0(x)/E3
   if(withplot)
       { dev.new() #windows()
         plot(x.seq, psi.01(x.seq),
                     type = if(is(distr,"DiscreteDistribution")) "p" else "l")
       }
   E4 <- E(object=distr, fun = function(x) psi.01(x)^2)
   psi.01 <- EuclRandVariable(Map = list(psi.01), Domain = Reals())

#   print(list(E2,E4,E2-E4))

      }else{

   ## multivariate case

   Dim <- length(evalRandVar(L2deriv, 1))

   ## Delta, formula (56), p. 133 [Ri:94]
   ##        Ptheta- primitive function for Lambda

   Map.Delta <- vector("list",Dim)
  # print("HLL")
  # print(x.seq0)
   for(i in 1:Dim)
       { if(is(distr,"AbscontDistribution")){
            #print(L2deriv@Map[[i]])
            fct0 <- sapply(x.seq0, L2deriv@Map[[i]]) * 
                           d(distr)(x.seq0)
            #print(fct0)
            Delta0 <-  h0*.csimpsum(fct0)   
         }else{
            fct0 <- function(x,y) L2deriv@Map[[i]](x)*(x<=y)
            Delta0 <- sapply(x.seq, function(Y){ fct <- function(x) fct0(x,y=Y)
                                            return(E(object=distr, fun = fct))})
         }         
         #print(Delta0)
         Delta1 <- approxfun(x.seq, Delta0, yleft = 0, yright = 0)
         if(is(distr,"DiscreteDistribution"))
               Delta <- function(x) Delta1(x) * (x %in% support(distr))
         else  Delta <- function(x) Delta1(x)
         Map.Delta[[i]] <- Delta
         env.i <- environment(Map.Delta[[i]]) <- new.env()
         assign("i", i, envir=env.i)
         assign("fct", fct, envir=env.i)
         assign("fct0", fct0, envir=env.i)
         assign("Delta", Delta, envir=env.i)
         assign("Delta0", Delta0, envir=env.i)
         assign("Delta1", Delta1, envir=env.i)
         if(withplot){ 
           dev.new()
           #windows()
           plot(x.seq, sapply(x.seq,Map.Delta[[i]]),
                     type = if(is(distr,"DiscreteDistribution")) "p" else "l")
         }

   }
   Delta <-  EuclRandVariable(Map = Map.Delta, Domain = Reals())



   ## J = Var_Ptheta Delta
   J1 <- E(object=distr, fun = Delta)
   Delta.0 <- Delta - J1
   J <- E(object=distr, fun = Delta.0 %*%t(Delta.0))
   ### CvM-IC phi
   phi <- as(solve(J)%*%Delta.0,"EuclRandVariable")

   ## integrand phi x Ptheta in formula (51) [ibid]

   Map.phi1 <- vector("list",Dim)
   for(i in 1:Dim)
       { Map.phi1[[i]] <- function(x) evalRandVar(phi,x)[i] * p(distr)(x)
         env.i <- environment(Map.phi1[[i]]) <- new.env()
         assign("i", i, envir=env.i)
         }

   phi1 <- EuclRandVariable(Map = Map.phi1, Domain = Reals())
   psi1 <- E(object=mu, fun = phi1)


   ## obtaining IC psi  (formula (51))
   Map.psi <- vector("list",Dim)
   for(i in 1:Dim)
     { if(is(mu,"AbscontDistribution")){
            fct01 <- function(x) phi@Map[[i]](x)*d(mu)(x)
            fct0 <-  sapply(rev(x.mu.seq0),fct01)
            phi0 <-  h0.mu*rev(.csimpsum(fct0))   
       }else{
            fct01 <- NULL
            fct0 <- function(x,y) evalRandVar(phi, y)[i]*(x<=y)
            phi0 <- sapply(x.mu.seq, 
                           function(X){ 
                               fct <- function(y) fct0(x = X, y)
                               return(E(object = mu, fun = fct))
                               })
       }
              
       phi0a <- approxfun(x.mu.seq, phi0, yleft = 0, yright = rev(phi0)[1])
       env.i <- environment(phi1) <- new.env()
       assign("i", i, envir=env.i)
       if(is(distr,"DiscreteDistribution"))
             psi0 <- function(x) phi0a(x) * (x %in% support(mu))
       else  psi0 <- function(x) phi0a(x)

       Map.psi[[i]] <- psi0
       env.i <- environment(Map.psi[[i]]) <- new.env()
       assign("i", i, envir=env.i)
       assign("fct", fct, envir=env.i)
       assign("fct0", fct0, envir=env.i)
       assign("psi0", psi0, envir=env.i)
       assign("phi0a", phi0a, envir=env.i)
       assign("phi0", phi0, envir=env.i)
    }
   psi <-  EuclRandVariable(Map = Map.psi, Domain = Reals())

   E2 <- E(object=distr, fun = psi %*%t(psi))   
   ## E2 = Cov_mu (psi)

   ### control: centering & standardization
   L2deriv <- L2Fam@L2deriv[[1]]
   E1 <- E(object=distr, fun = psi )
   E3 <- E(object=distr, fun = psi %*%t(L2deriv))
   psi.0 <- psi - E1
   psi.01 <- as(solve(E3)%*%psi.0,"EuclRandVariable")
   if(withplot)
      { for(i in 1:Dim)
         { dev.new()
           plot(x.mu.seq, sapply(x.mu.seq,psi.01@Map[[i]]),
                     type = if(is(distr,"DiscreteDistribution")) "p" else "l")
         }}
   E4 <- E(object=distr, fun = psi.01 %*%t(psi.01))
   }
  E4 <- PosSemDefSymmMatrix(E4)
  
  psi <-  EuclRandVarList(psi.01)
  nms <- names(c(main(param(L2Fam)),nuisance(param(L2Fam))))
  dimnames(E4) = list(nms,nms)
  if(withpreIC) return(list(preIC=psi, Var=E4))
  else return(E4)
}

### examples:
if(FALSE){
P0 <- PoisFamily();.CvMMDCovariance(P0,par=ParamFamParameter("lambda",1), withplot=TRUE)
B0 <- BinomFamily(size=8, prob=0.3);.CvMMDCovariance(B0,par=ParamFamParameter("",.3), withplot=TRUE)
N0 <- NormLocationFamily();.CvMMDCovariance(N0,par=ParamFamParameter("",0), withplot=TRUE, N = 200)
C0 <- L2LocationFamily(central=Cauchy());.CvMMDCovariance(C0,par=ParamFamParameter("",0), withplot=TRUE, N = 200)
N1 <- NormScaleFamily(); re=.CvMMDCovariance(N1,par=ParamFamParameter("",1), withICwithplot=TRUE, N = 200)
NS <- NormLocationScaleFamily();paramP <- ParamFamParameter(name = "locscale", main = c("loc"=0,"scale"=1),trafo = diag(2));
      .CvMMDCovariance(NS,par=paramP, withplot=TRUE, N = 100)
cls <- CauchyLocationScaleFamily();.CvMMDCovariance(cls,par=ParamFamParameter("",0:1), withplot=TRUE, N = 200)
Els <- L2LocationScaleFamily(loc = 0, scale = 1,
                  name = "Laplace Location and scale family",
                  centraldistribution = DExp(),
                  LogDeriv = function(x)  sign(x),
                  FisherInfo = diag(2),
                  trafo = diag(2))
.CvMMDCovariance(Els,par=ParamFamParameter("",0:1), withplot=TRUE, N = 100)

system.time(print(.CvMMDCovariance(P0,par=ParamFamParameter("lambda",1))))
system.time(print(.CvMMDCovariance(B0,par=ParamFamParameter("",.3))))
system.time(print(.CvMMDCovariance(N0,par=ParamFamParameter("",0), N = 100)))
system.time(print(.CvMMDCovariance(C0,par=ParamFamParameter("",0), N = 100)))
system.time(print(.CvMMDCovariance(N1,par=ParamFamParameter("",1), N = 100)))
system.time(print(.CvMMDCovariance(NS,par=paramP, N = 100)))
system.time(print(.CvMMDCovariance(cls,par=ParamFamParameter("",0:1), N = 100)))
system.time(print(.CvMMDCovariance(Els,par=ParamFamParameter("",0:1), N = 100)))

}

#------------------------------------
#### utilities copied from package distr v.2.6  svn-rev 943
#------------------------------------
.inArgs <- function(arg, fct)
          {as.character(arg) %in% names(formals(fct))}

.isEqual <- function(p0, p1, tol = min( getdistrOption("TruncQuantile")/2,
                                          .Machine$double.eps^.7
                                          ))
                abs(p0-p1)< tol

.csimpsum <- function(fx){
 l <- length(fx)
 l2 <- l%/%2
 if (l%%2 == 0) {
     fx <- c(fx[1:l2],(fx[l2]+fx[l2+1])/2,fx[(l2+1):l])
     l <- l+1}
 f.even <- fx[seq(l) %% 2 == 0]
 f.odd  <- fx[seq(l) %% 2 == 1]
 fs    <- 2 * cumsum(f.odd) - f.odd - f.odd[1]
 fsm   <- 4 * cumsum(f.even)
 ff <- c(0,(fs[2:(l2+1)]+fsm)/3 )
 ff
}

.List <- function(list0) if(is.list(list0)) list0 else list(list0)

.fillList <- function(list0, len = length(list0)){
            if(is.null(list0)) return(vector("list",len))
            list0 <- .List(list0)
            if(len == length(list0))
               return(list0)
            i <- 0
            ll0 <- length(list0)
            li0 <- vector("list",len)
            if(ll0)
            while(i < len){
               j <- 1 + ( i %% ll0)
               i <- i + 1
               li0[[i]] <- list0[[j]]
            }
           return(li0)
}

.confqq <- function(x,D, datax = TRUE, withConf.pw  = TRUE,
                    withConf.sim = TRUE, alpha,
                    col.pCI, lty.pCI, lwd.pCI, pch.pCI, cex.pCI,
                    col.sCI, lty.sCI, lwd.sCI, pch.sCI, cex.sCI,
                    n,exact.sCI=(n<100),exact.pCI=(n<100), nosym.pCI = FALSE,
                    with.legend = TRUE, legend.bg = "white",
                    legend.pos = "topleft", legend.cex = 0.8,
                    legend.pref = "", legend.postf = "",
                    legend.alpha = alpha, qqb0=NULL, transf0=NULL, debug = FALSE){

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
   tx.c <- if(is.null(transf0)) x.c else transf0(x.c)
   tx.d <- if(is.null(transf0)) x.d else transf0(x.d)


   qqb <- if(is.null(qqb0)) qqbounds(x,D,alpha,n,withConf.pw, withConf.sim,
                   exact.sCI,exact.pCI,nosym.pCI, debug) else qqb0

   qqb$crit <- qqb$crit[SI.in,]

   if(qqb$err["pw"]){
      if(sum(SI.c)>0){
         if(!datax){
            lines(tx.c, qqb$crit[SI.c,"pw.right"],
               col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
            lines(tx.c, qqb$crit[SI.c,"pw.left"],
               col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
         }else{
            lines(qqb$crit[SI.c,"pw.right"], tx.c,
               col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
            lines(qqb$crit[SI.c,"pw.left"], tx.c,
               col=col.pCI,lty=lty.pCI,lwd=lwd.pCI)
         }
      }
      if(sum(!SI.c)>0){
         if(!datax){
            points(tx.d, qqb$crit[!SI.c,"pw.right"],
               col=col.pCI, pch=pch.pCI, cex = cex.pCI)
            points(tx.d, qqb$crit[!SI.c,"pw.left"],
               col=col.pCI, pch=pch.pCI, cex = cex.pCI)
         }else{
            points(qqb$crit[!SI.c,"pw.right"], tx.d,
               col=col.pCI, pch=pch.pCI, cex = cex.pCI)
            points(qqb$crit[!SI.c,"pw.left"], tx.d,
               col=col.pCI, pch=pch.pCI, cex = cex.pCI)
         }
      }
   }
   if(qqb$err["sim"]){
      if(sum(SI.c)>0){
         if(!datax){
            lines(tx.c, qqb$crit[SI.c,"sim.right"],
               col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
            lines(tx.c, qqb$crit[SI.c,"sim.left"],
               col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
         }else{
            lines(qqb$crit[SI.c,"sim.right"], tx.c,
               col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
            lines(qqb$crit[SI.c,"sim.left"], tx.c,
               col=col.sCI,lty=lty.sCI,lwd=lwd.sCI)
         }
      }
      if(sum(!SI.c)>0){
         if(!datax){
            points(tx.d, qqb$crit[!SI.c,"sim.right"],
                col=col.sCI, pch=pch.sCI, cex = cex.sCI)
            points(tx.d, qqb$crit[!SI.c,"sim.left"],
                col=col.sCI, pch=pch.sCI, cex = cex.sCI)
         }else{
            points(qqb$crit[!SI.c,"sim.right"], tx.d,
                col=col.sCI, pch=pch.sCI, cex = cex.sCI)
            points(qqb$crit[!SI.c,"sim.left"], tx.d,
                col=col.sCI, pch=pch.sCI, cex = cex.sCI)
         }
      }
   }
   if(with.legend){
      if( qqb$err["pw"] ||  qqb$err["sim"] ){
         expression1 <- substitute(
            legpf~nosym0~"pointw."~ex.p~alpha==alpha0~"%- conf. interval"~legpof,
            list(legpf = legend.pref, legpof = legend.postf,
                 ex.p = if(exact.pCI) "exact" else "asympt.",
                 alpha0 = round(legend.alpha*100,2),
                 nosym0 = if(nosym.pCI&&exact.pCI) "shortest asymm." else "symm"))
         expression2 <- substitute(
            legpf~"simult."~ex.s~alpha==alpha0~"%- conf. interval"~legpof,
            list(legpf = legend.pref, legpof = legend.postf,
                 ex.s = if(exact.sCI) "exact" else "asympt.",
                 alpha0 = round(legend.alpha*100,2)))

         lcl <- list()
         if(!qqb$err["sim"]){
            expression3 <- expression1
            lcl$pch <- if(sum(!SI.c)>0) pch.pCI else NULL
            lcl$lty <- if(sum(SI.c)>0)  lty.pCI else NULL
            lcl$col <- col.pCI
            lcl$lwd <- if(sum(SI.c)>0)  2 else NULL
         }
         if(!qqb$err["pw"]){
            expression3 <- expression2
            lcl$pch <- if(sum(!SI.c)>0) pch.sCI else NULL
            lcl$lty <- if(sum(SI.c)>0)  lty.sCI else NULL
            lcl$col <- col.sCI
            lcl$lwd <- if(sum(SI.c)>0)  2 else NULL
         }
         if( qqb$err["pw"] && qqb$err["sim"]){
            expression3 <- eval(substitute(expression(expression1, expression2)))
            lcl$pch <- if(sum(!SI.c)>0) c(pch.pCI, pch.sCI) else NULL
            lcl$lty <- if(sum(SI.c)>0)  c(lty.pCI, lty.sCI) else NULL
            lcl$col <- c(col.pCI,col.sCI)
            lcl$lwd <- if(sum(SI.c)>0)  2 else NULL
         }
         do.call(legend, c(list(legend.pos, legend = expression3, bg = legend.bg,
                                merge = FALSE, cex = legend.cex), lcl))
      }
   }
  return(invisible(qqb))
}

.deleteItemsMCL <- function(mcl){
    mcl$datax <- mcl$n <- NULL
    mcl$col.IdL <- mcl$alpha.CI <- mcl$lty.IdL <-  NULL
    mcl$col.NotInSupport <- mcl$check.NotInSupport <- NULL
    mcl$exact.sCI <- mcl$exact.pCI <- NULL
    mcl$withConf <- mcl$withConf.sim <- mcl$withConf.pw <- NULL
    mcl$withIdLine <- mcl$distance <- NULL
    mcl$col.pCI <- mcl$lty.pCI <- mcl$col.sCI <- mcl$lty.sCI <- NULL
    mcl$lwd.IdL <- mcl$lwd.pCI <- mcl$lwd.sCI <- NULL
    mcl$withLab <- mcl$lab.pts <- mcl$which.lbs <- NULL
    mcl$which.Order <- mcl$order.traf  <- NULL
    mcl$col.pch <- mcl$cex.pch  <- mcl$jit.fac <- NULL
    mcl$col.lbl <- mcl$cex.lbl  <- mcl$adj.lbl <- NULL
    mcl$exp.cex2.pch <- mcl$exp.cex2.lbl <- NULL
    mcl$exp.fadcol.pch <- mcl$exp.fadcol.lbl <- NULL
    mcl$nosym.pCI <- mcl$n.CI <- mcl$n.adj <- NULL
    mcl$legend.cex <- mcl$with.legend <- mcl$legend.bg <- NULL
    mcl$legend.pos <- mcl$legend.pref <- mcl$legend.postf <- NULL
    mcl$legend.alpha <- NULL
    mcl$withSweave <- NULL
    mcl$mfColRow <- NULL
    mcl$debug <- NULL
    mcl$added.points.CI <- NULL

mcl}

## helpers
.inGaps <- function(x,gapm){
  if(is.null(gapm)) return(rep(FALSE,length(x)))
  fct <- function(x,m){ m[,2]>=x & m[,1]<=x}
  sapply(x, function(y) length(which(fct(y,gapm)))>0)
}

.isReplicated <- function(x, tol=.Machine$double.eps){
  tx <- table(x)
  rx <- as.numeric(names(tx[tx>1]))
  sapply(x, function(y) any(abs(y-rx)<tol))
}

.NotInSupport <- function(x,D){
  if(length(x)==0) return(logical(0))
  nInSupp <- which(x < q.l(D)(0))
  nInSupp <- unique(sort(c(nInSupp,which(x > q.l(D)(1)))))

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

.SingleDiscrete <- function(x,D){
  ## produces a logical vector of
  ##     0  : discrete mass point
  ##     1  : within continuous support
  ##     2  : left gap point
  ##     3  : right gap point
  ##     4  : not in support
  lx <- x * 0

  lx[.NotInSupport(x,D)] <- 4

  idx.0 <- ((x>q.l(D)(1)) | (x<q.l(D)(0)))
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

#------------------------------------------------------------------------------
# .presubs : for titles etc
#------------------------------------------------------------------------------

.presubs <- function(inp, frompat, topat){
### replaces in an expression or a string all frompat patterns to topat patterns

logic <- FALSE
inCx <- sapply(inp,
   function(inpx){
      inC <- deparse(inpx)
      l <- length(frompat)
      for(i in 1:l)
         { if (is.language(topat[[i]])){
               totxt <- deparse(topat[[i]])
               totxt <- gsub("expression\\(", "\", ", gsub("\\)$",", \"",totxt))
               if (length(grep(frompat[i],inC))) logic <<- TRUE
               inC <- gsub(frompat[i],totxt,inC)
           }else inC <- gsub(frompat[i], topat[[i]], inC)
         }
      return(inC)
    })
if(length(grep("expression",inCx))>0)
   inCx <- gsub("expression\\(", "", gsub("\\)$","",inCx))
if (length(inCx) > 1) {
   inCx <- paste(inCx, c(rep(",", length(inCx)-1), ""),
                 sep = "", collapse = "\"\\n\",")
   if ( any(as.logical(c(lapply(inp,is.language)))) | logic )
      inCx <- paste("expression(paste(", gsub("\\\\n"," ", inCx), "))", sep ="")
   else
      inCx <- paste("paste(",inCx,")", sep ="")
}else inCx <- paste("expression(paste(",inCx,"))",sep="")
outC <- eval(parse(text = eval(inCx)))
return(outC)
}

.panel.mingle <- function(dots, element){
  pF <- dots[[element]]
  if(is.list(pF)) return(pF)
  pFr <- if(typeof(pF)=="symbol") eval(pF) else{
     pFc <- as.call(pF)
     if(as.list(pFc)[[1]] == "list"){
        lis <- vector("list",length(as.list(pFc))-1)
        for(i in 1:length(lis)){
            lis[[i]] <- pFc[[i+1]]
        }
        lis
     }else pF
  }
  return(pFr)
}


#---------------------------------------------------
### from packages stats:
#---------------------------------------------------
format.perc <- function (probs, digits)
paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
    "%")
