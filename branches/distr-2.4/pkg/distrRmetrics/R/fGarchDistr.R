SNorm <- function(mean=0, sd=1, xi=1.5){
  m0 <- mean; sd0 <- sd; xi0 <- xi
  new("SNorm",r=function(n)rsnorm(n,mean=m0,sd=sd0,xi=xi0),
                      d=function(x, log=FALSE){
                          d0 <- dsnorm(x,mean=m0,sd=sd0,xi=xi0)
                          return(if(log) log(d0) else d0)
                          },
                      p=function(q, lower.tail=TRUE, log.p=FALSE){
                          p00 <- psnorm(q,mean=m0,sd=sd0,xi=xi0)
                          p0  <- if(lower.tail) p00 else 1-p00
                          return(if(log.p) log(p0) else p0)
                          },
                      q=function(p, lower.tail=TRUE, log.p=FALSE){
                          p00 <- if(log.p) exp(p) else p
                          p0 <- if(lower.tail) p00 else 1-p00
                          return(qsnorm(p0,mean=m0,sd=sd0,xi=xi0))
                          },
                      param = new("SNormParameter", mean=m0,sd=sd0,xi=xi0))
}
STd <- function(mean=0, sd=1, nu=5)
  Td(ncp=0,df=nu,location=mean,scale=sqrt((nu-2)/nu)*sd0)

SSTd <- function(mean=0, sd=1, nu=5, xi=1.5){
  m0 <- mean; sd0 <- sd; nu0 <- nu; xi0 <- xi
  new("SSTd",r=function(n)rsstd(n,mean=m0,sd=sd0,nu=nu0,xi=xi0),
                      d=function(x, log=FALSE){
                          d0 <- dsstd(x,mean=m0,sd=sd0,nu=nu0,xi=xi0)
                          return(if(log) log(d0) else d0)
                          },
                      p=function(q, lower.tail=TRUE, log.p=FALSE){
                          p00 <- psstd(q,mean=m0,sd=sd0,nu=nu0,xi=xi0)
                          p0  <- if(lower.tail) p00 else 1-p00
                          return(if(log.p) log(p0) else p0)
                          },
                      q=function(p, lower.tail=TRUE, log.p=FALSE){
                          p00 <- if(log.p) exp(p) else p
                          p0 <- if(lower.tail) p00 else 1-p00
                          return(qsstd(p0,mean=m0,sd=sd0,nu=nu0,xi=xi0))
                          },
                      param = new("SSTdParameter", mean=m0,sd=sd0,nu=nu0,xi=xi0),
                      )
}

## Access methods
setMethod("xi", signature(x = "SNormParameter"), function(x, ...) x@xi)
setMethod("xi", signature(x = "SSTdParameter"), function(x, ...) x@xi)
setMethod("nu", signature(x = "SSTdParameter"), function(x, ...) x@nu)
## wrapped access methods
setMethod("mean", "SNorm", function(x, ...) mean(param(x)))
setMethod("mean", "SSTd", function(x, ...) mean(param(x)))
setMethod("sd", signature(x = "Norm"), function(x) sd(param(x)))
setMethod("sd", signature(x = "SSTd"), function(x) sd(param(x)))
setMethod("xi", signature(x = "SNorm"), function(x, ...) x@param@xi)
setMethod("xi", signature(x = "SSTd"), function(x, ...) x@param@xi)
setMethod("nu", signature(x = "SSTd"), function(x, ...) x@param@nu)
## Replace Methoden
setReplaceMethod("xi", "SNormParameter",
                  function(object, value)
                      { object@xi <- value; object})
setReplaceMethod("xi", "SSTdParameter",
                  function(object, value)
                      { object@xi <- value; object})
setReplaceMethod("nu", "SSTdParameter",
                  function(object, value)
                      { object@nu <- value; object})
## wrapped replace methods
setMethod("mean<-", "SNorm",
           function(object, value) SNorm(mean=value, sd=sd(object), xi=xi(object)))
setMethod("mean<-", "SSTd",
           function(object, value) SSTd(mean=value, sd=sd(object), xi=xi(object)))
setMethod("sd<-", "SNorm",
           function(object, value) SNorm(mean=mean(object), sd=value, xi=xi(object)))
setMethod("sd<-", "SSTd",
           function(object, value) SSTd(mean=mean(object), sd=value, xi=xi(object), nu =nu(object)))
setMethod("nu<-", "SSTd",
           function(object, value) SSTd(mean=mean(object), sd=sd(object), nu=value, xi=xi(object)))
setMethod("xi<-", "SNorm",
           function(object, value) SSTd(mean=mean(object), sd=sd(object), xi=value))
setMethod("xi<-", "SSTd",
           function(object, value) SSTd(mean=mean(object), sd=sd(object), xi=value, nu =nu(object)))
