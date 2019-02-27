## checks for change in .qmodifygaps
require(distr)
d0 <- function(x) dunif(abs(x),min=0.5,max=1)/2
DC <- AbscontDistribution(d=d0)
gaps(DC)

pv <- c(0.499,.4999,.5,.5001,.501)
q.l(DC)(pv,lower=TRUE)
q.l(DC)(pv,lower=FALSE)
q.r(DC)(pv,lower=TRUE)
q.r(DC)(pv,lower=FALSE)

require(distr)
X=Unif(2,3)
Y=Pois(lambda=3)
Z=X+Y
V=Z+sin(Norm())

Mi <- Minimum(X,X)
