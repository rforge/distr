################################################################
# QQ - Plot helper functions in package distr enhanced by distrEx
################################################################


assignInNamespace(".q2pw", value=function(x,p.b,D,n,alpha,exact=(n<100),nosym=FALSE){
 if(exact){
    fct <- if(nosym) .BinomCI.nosym else .BinomCI
    ro <- fct(x,p.b,D,n,alpha)
    return(ro)
 }
 pq <- log(p.b)+log(1-p.b)
 if(is(D,"AbscontDistribution")){
    dp <- d(D)(x,log=TRUE)
    dsupp.p <- dsupp.m<-1
 }else{ ## have E and sd available
    supp.ind <- sapply(x, function(y)
                 which.min(abs(y-support(D))))
    nx <- length(support(D))
    supp.ind.p <- pmax(supp.ind + 1 ,nx)
    supp.ind.m <- pmax(supp.ind - 1 ,1)
    dsupp.p <- support(D)[supp.ind.p] - support(D)[supp.ind]
    dsupp.m <- support(D)[supp.ind] - support(D)[supp.ind.m]
    s <- sd(D)
    m <- E(D)
    dp <- log(pnorm((x+dsupp.p/2-m)/s) - pnorm((x-dsupp.m/2-m)/s))
 }
 ro <- exp(pq/2-dp)*(dsupp.p+dsupp.m)/2*qnorm((1+alpha)/2)
 return(cbind(left=-ro,right=ro))
}, ns="distr")



