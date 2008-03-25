require(distr)                                 

if(!isGeneric("Minimum")) 
    setGeneric("Minimum", 
    function(e1, e2) standardGeneric("Minimum"))

setMethod("Minimum",
          signature(e1 = "AbscontDistribution", 
          e2 = "AbscontDistribution"),
          function(e1, e2){
            ## new random number function
            rnew <- function(n){
              rn1 <- r(e1)(n)
              rn2 <- r(e2)(n)
              
              ifelse(rn1 < rn2, rn1, rn2)
            }

            ## new cdf 
            pnew <- function(x){
              p1 <- p(e1)(x)
              p2 <- p(e2)(x)
              p1 + p2 - p1 * p2
            }

            ## new density 
            dnew <- function(x){
              d1 <- d(e1)(x)
              d2 <- d(e2)(x)
              p1 <- p(e1)(x)
              p2 <- p(e2)(x)
              d1 + d2 - d1 * p2 - p1 * d2
            }

            ## new quantile function
            lower1 <- q(e1)(0)
            lower2 <- q(e2)(0)
            upper1 <- q(e1)(1)
            upper2 <- q(e2)(1)
            lower <- min(lower1, lower2)
            upper <- min(upper1, upper2)

            maxquantile = min(q(e1)(1e-6, lower.tail = FALSE), 
                              q(e2)(1e-6, lower.tail = FALSE))
            minquantile = min(q(e1)(1e-6), 
                              q(e2)(1e-6)) 
            
            qfun1 <- function(x){
              if(x == 0) return(lower)
              if(x == 1) return(upper)
              fun <- function(t) pnew(t) - x
              uniroot(f = fun, 
                  interval = c(maxquantile, 
                               minquantile))$root
            }
            qfun2 <- function(x)
              sapply(x, qfun1)

            return(new("AbscontDistribution", r = rnew, 
                   d = dnew, p = pnew, q = qfun2))            
          })

setMethod("Minimum",
          signature(e1 = "AbscontDistribution", 
          e2 = "numeric"),
          function(e1, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")
            
            ## new random number function
            
            
            rnew <- function(n){
              rn1 <- matrix(r(e1)(n*e2),n,e2)
              apply(rn1,1,min) 
            }

            ## new cdf  
            pnew <- function(x){
              1 - (p(e1)(x, lower.tail = FALSE))^e2
            }

            ## new density 
            dnew <- function(x){
              e2 * (p(e1)(x, lower.tail = FALSE))^(e2-1) * (d(e1)(x))
            }

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
                  interval = c(maxquantile, 
                               minquantile))$root
            }
            qfun2 <- function(x)
              sapply(x, qfun1)

            return(new("AbscontDistribution", r = rnew, 
                   d = dnew, p = pnew, q = qfun2))            
          })


if(!isGeneric("Maximum")) setGeneric("Maximum", 
    function(e1, e2) standardGeneric("Maximum"))

setMethod("Maximum",
          signature(e1 = "AbscontDistribution", 
                    e2 = "AbscontDistribution"),
          function(e1, e2){
            ## new random number function
            rnew <- function(n){
              rn1 <- r(e1)(n)
              rn2 <- r(e2)(n)
              
              ifelse(rn1 > rn2, rn1, rn2)
            }

            ## new cdf 
            pnew <- function(x){
              p1 <- p(e1)(x)
              p2 <- p(e2)(x)
              p1 * p2
            }

            ## new density 
            dnew <- function(x){
              d1 <- d(e1)(x)
              d2 <- d(e2)(x)
              p1 <- p(e1)(x)
              p2 <- p(e2)(x)
              d1 * p2 + p1 * d2
            }

            ## new quantile function
            lower1 <- q(e1)(0)
            lower2 <- q(e2)(0)
            upper1 <- q(e1)(1)
            upper2 <- q(e2)(1)
            lower <- max(lower1, lower2)
            upper <- max(upper1, upper2)

            maxquantile = max(q(e1)(1e-6, lower.tail = FALSE), 
                              q(e2)(1e-6, lower.tail = FALSE))
            minquantile = max(q(e1)(1e-6), q(e2)(1e-6)) 
            
            qfun1 <- function(x){
              if(x == 0) return(lower)
              if(x == 1) return(upper)
              fun <- function(t) pnew(t) - x
              uniroot(f = fun, interval = c(maxquantile, 
                                        minquantile))$root
            }
            qfun2 <- function(x)
              sapply(x, qfun1)
            
            return(new("AbscontDistribution", r = rnew, 
                   d = dnew, p = pnew, q = qfun2))            
          })

setMethod("Maximum",
          signature(e1 = "AbscontDistribution", 
          e2 = "numeric"),
          function(e1, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")
            
            ## new random number function
            
            
            rnew <- function(n){
              rn1 <- matrix(r(e1)(n*e2),n,e2)
              apply(rn1,1,max) 
            }

            ## new cdf  
            pnew <- function(x){
              p(e1)(x)^e2
            }

            ## new density 
            dnew <- function(x){
              e2 * (p(e1)(x))^(e2-1) * (d(e1)(x))
            }

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
                  interval = c(maxquantile, 
                               minquantile))$root
            }
            qfun2 <- function(x)
              sapply(x, qfun1)

            return(new("AbscontDistribution", r = rnew, 
                   d = dnew, p = pnew, q = qfun2))            
          })


# Example

N <- Norm(mean = 0, sd = 1)
U <- Unif(Min = 0, Max = 1)

Y <- Maximum(N, U)
plot(Y)

cat("Hit <enter> to continue...")
readline()

Y0 <- Maximum(N, 10)
plot(Y0)

cat("Hit <enter> to continue...")
readline()

Z <- Minimum(N,U)
plot(Z)

cat("Hit <enter> to continue...")
readline()

Z0 <- Maximum(N, 10)
plot(Z0)

cat("Hit <enter> to continue...")
readline()

setMethod("Minimum", signature(e1 = "DiscreteDistribution", 
                               e2 = "DiscreteDistribution"),
    function(e1, e2){
        ## new support
        supp1 <- support(e1)
        supp2 <- support(e2)
        suppMax <- min(max(supp1), max(supp2))
        supp <- union(supp1[supp1 <= suppMax], supp2[supp2 <= suppMax])
        len <- length(supp)
        if(length(usupp <- unique(supp)) < len){
            warning("collapsing to unique support values")
            supp <- sort(usupp)
            len <- length(supp)
        }else{
            o <- order(supp)
            supp <- supp[o]
        }
        
        ## new random number function
        rnew <- function(n){
            rn1 <- r(e1)(n)
            rn2 <- r(e2)(n)      
            ifelse(rn1 < rn2, rn1, rn2)
        }

        ## new cdf 
        pnew <- function(x){
          p2 <- p(e2)(x)
          p(e1)(x)*(1 - p2) + p2
        }

        ## new density 
        dnew <- function(x){
          d1 <- d(e1)(x)
          d2 <- d(e2)(x)
          d1*(1-p(e2)(x)) + d2*(1 - p(e1)(x)) + d1*d2
        }

        ## new quantile function  
        cumprob <- pnew(supp)
        qnew <- function(x){ supp[sum(cumprob<x)+1] }        

        return(new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, 
                   q = qnew, support = supp))            
    })

setMethod("Minimum", signature(e1 = "DiscreteDistribution", 
                               e2 = "numeric"),
    function(e1, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")

        ## new support
        supp <- support(e1)
        len <- length(supp)

        
        ## new random number function
        rnew <- function(n){
              rn1 <- matrix(r(e1)(n*e2),n,e2)
              apply(rn1,1,min) 
        }

        ## new cdf 
        pnew <- function(x){
              1 - (p(e1)(x, lower.tail = FALSE))^e2
        }

        ## new density 
        ## P(m=x)=P(m<=x)-P(m<x)
        dnew <- function(x){
             (pnew(x)-pnew(x-getdistrOption("DistrResolution")))*(d(e1)(x)>0)
        }

        ## new quantile function  
        cumprob <- pnew(supp)
        qnew <- function(x){ supp[sum(cumprob<x)+1] }        

        return(new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, 
                   q = qnew, support = supp))            
    })

## Implementation of Maximum
setMethod("Maximum", signature(e1 = "DiscreteDistribution", 
                               e2 = "DiscreteDistribution"),
    function(e1, e2){
        ## new support
        supp1 <- support(e1)
        supp2 <- support(e2)
        suppMin <- max(min(supp1), min(supp2))
        supp <- union(supp1[supp1 >= suppMin], supp2[supp2 >= suppMin])
        len <- length(supp)
        if(length(usupp <- unique(supp)) < len){
            warning("collapsing to unique support values")
            supp <- sort(usupp)
            len <- length(supp)
        }else{
            o <- order(supp)
            supp <- supp[o]
        }
        
        ## new random number function
        rnew <- function(n){
            rn1 <- r(e1)(n)
            rn2 <- r(e2)(n)      
            ifelse(rn1 > rn2, rn1, rn2)
        }

        ## new cdf 
        pnew <- function(x){ p(e1)(x)*p(e2)(x) }

        ## new density 
        dnew <- function(x){
          d1 <- d(e1)(x)
          d2 <- d(e2)(x)
          p(e1)(x)*d2 + p(e2)(x)*d1 - d1*d2
        }

        ## new quantile function  
        cumprob <- pnew(supp)
        qnew <- function(x){ supp[sum(cumprob<x)+1] }        

        return(new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, 
                   q = qnew, support = supp))            
    })

setMethod("Maximum", signature(e1 = "DiscreteDistribution", 
                               e2 = "numeric"),
    function(e1, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")

        ## new support
        supp <- support(e1)
        len <- length(supp)

        
        ## new random number function
        rnew <- function(n){
              rn1 <- matrix(r(e1)(n*e2),n,e2)
              apply(rn1,1,max) 
        }

        ## new cdf 
        pnew <- function(x){
              (p(e1)(x))^e2
        }

        ## new density 
        ## P(m=x)=P(m<=x)-P(m<x)
        dnew <- function(x){
             (pnew(x)-pnew(x-getdistrOption("DistrResolution")))*(d(e1)(x)>0)
        }

        ## new quantile function  
        cumprob <- pnew(supp)
        qnew <- function(x){ supp[sum(cumprob<x)+1] }        

        return(new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, 
                   q = qnew, support = supp))            
    })

B1 <- Binom(6, 0.5)
B2 <- Binom(6, 0.5)

C <- Maximum(B1, B2)
plot(C)

cat("Hit <enter> to continue...")
readline()

C0 <- Maximum(B1, 10)
plot(C0)

cat("Hit <enter> to continue...")
readline()

D <- Minimum(B1, B2)
plot(D)

cat("Hit <enter> to continue...")
readline()

D0 <- Minimum(B1, 10)
plot(D0)

