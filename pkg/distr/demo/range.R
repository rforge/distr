require(distr)

if(!isGeneric("Range"))
    setGeneric("Range",
    function(e1, e2) standardGeneric("Range"))
    

setMethod("Range",
          signature(e1 = "AbscontDistribution",
          e2 = "numeric"),
          function(e1, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")

            ## new random number function
            rnew <- function(n){
              rn1 <- matrix(r(e1)(n*e2),n,e2)
              m <- apply(rn1,1,min)
              M <- apply(rn1,1,max)
              M-m
            }

            fnt <- function(u0,s) d(e1)(s)*(p(e1)(s+u0)-p(e1)(s))^(e2-1)*e2
            fu <- function(u) integrate(fnt, lower=-Inf, upper=Inf, u0=u)$value*(u>0)
            fnt0 <- function(u0,s) d(e1)(s)*d(e1)(s+u0)*(p(e1)(s+u0)-p(e1)(s))^(e2-2)*e2*(e2-1)
            fu0 <- function(u) integrate(fnt0, lower=-Inf, upper=Inf, u0=u)$value*(u>0)
            xgrid <- seq(0,
                         q(e1)(1e-6, lower.tail = FALSE)-q(e1)(1e-6),
                         length = getdistrOption("DefaultNrGridPoints")/10)
            fx <- sapply(xgrid, fu)
            pnew <- approxfun(xgrid, fx, yleft = 0, yright = 1)
            fx0 <- sapply(xgrid, fu0)
            dnew <- approxfun(xgrid, fx0, yleft = 0, yright = 0)

            ## new quantile function
            lower <- q(e1)(0)
            upper <- q(e1)(1)

            maxquantile = q(e1)(1e-6, lower.tail = FALSE)
            minquantile = q(e1)(1e-6)

            qfun1 <- function(x){
              if(x == 0) return(lower)
              if(x == 1) return(upper)
              fun <- function(t) pnew(t) - x
              uniroot(f = fun,
                  interval = c(0,
                               maxquantile-minquantile))$root
            }
            qfun2 <- function(x)
              sapply(x, qfun1)

            return(new("AbscontDistribution", r = rnew,
                   d = dnew, p = pnew, q = qfun2))
          })

N <- Norm(mean = 0, sd = 1)

R <- Range(N, 5)
plot(R)

cat("Hit <enter> to continue...")
readline()

setMethod("Range",
          signature(e1 = "DiscreteDistribution",
          e2 = "numeric"),
          function(e1, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")

            supp <- support(e1)
            suppnew <- sort(unique(as.vector(outer(supp,supp,"-"))))
            suppnew <- suppnew[suppnew>=0]
            print(suppnew)
            
            ## new random number function
            rnew <- function(n){
              rn1 <- matrix(r(e1)(n*e2),n,e2)
              m <- apply(rn1,1,min)
              M <- apply(rn1,1,max)
              M-m
            }

            fnt <- function(u0,s) (p(e1)(s+u0)-p(e1)(s)+d(e1)(s))^e2 -
                                  (p(e1)(s+u0)-p(e1)(s))^e2
            pnew <- function(x) sapply(x, function(u) sum(fnt(u, s = supp)))*(x>=0)
            dnew <- function(x){
             (pnew(x)-pnew(x-getdistrOption("DistrResolution")*100))*(d(e1)(x)>0)*(x>=0)
              }

            cumprob <- pnew(suppnew)
            qnew <- function(x){ suppnew[sum(cumprob<x)+1] }

            return(new("DiscreteDistribution", r = rnew, d = dnew, p = pnew,
                   q = qnew, support = suppnew))

          })

B1 <- Binom(6, 0.5)
R <- Range(B1, 2)
plot(R)
