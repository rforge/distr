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
      c.crit.i <- x.in + c.crit.i/sqrt(n)
      c.crit.i[SIi == 2 | SIi == 3] <- NA
      c.c[SI.in,3:4] <- c.crit.i
   }
   return(list(crit = c.c, err=c(sim=te.s,pw=te.i)))
}
