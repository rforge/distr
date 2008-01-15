###############################################################################
## Method: ContaminationSize
## size of contamination for two distributions
###############################################################################
setMethod("ContaminationSize", signature(e1 = "AbscontDistribution", 
                                         e2 = "AbscontDistribution"),
    function(e1, e2){
        lower <- min(q(e1)(1e-10), q(e2)(1e-10))
        upper <- max(q(e1)(1-1e-10), q(e2)(1-1e-10))
        lower <- min(q(e1)(1e-10), q(e2)(1e-10))
        upper <- max(q(e1)(1-1e-10), q(e2)(1-1e-10))
        x <- seq(from = lower, to = upper, length = 1e5)
        fct <- function(rad, x, dfun1, dfun2){
            return(min(dfun2(x) - (1-rad)*dfun1(x)))
        }
        res <- try(uniroot(f = fct, interval = c(-1e-3,1+1e-3), 
                    tol = .Machine$double.eps^0.25, x = x, 
                    dfun1 = d(e1), dfun2 = d(e2))$root, silent=TRUE)
        if(!is.numeric(res)){ 
            return(list(e1 = e1, e2 = e2, size.of.contamination = 1))
        }
        
        return(list(e1 = e1, e2 = e2, size.of.contamination = res))
#        fct <- function(x, e1, e2){
#            p1 <- p(e1)(x)
#            return((p1 != 0)*(log(p(e2)(x)) - log(p1)) + (p1 == 0))
#        }
#        res <- optimize(fct, lower = lower, upper = upper, e1 = e1, e2 = e2)$objective
#        res <- round(1 - exp(res),2)
#        names(res) <- "size of contamination"
#
#        return(res)
    })
setMethod("ContaminationSize", signature(e1 = "DiscreteDistribution", 
                                         e2 = "DiscreteDistribution"),
    function(e1, e2){
#        supp <- union(support(e1), support(e2))
#        fct <- function(rad, x, e1, e2){
#            return(min(d(e1)(x) - (1-rad)*d(e2)(x)))
#        }
#        res <- try(uniroot(f = fct, interval = c(-1e-3,1+1e-3), 
#                    tol = .Machine$double.eps^0.25, x = supp, 
#                    e1 = e1, e2 = e2)$root, silent=TRUE)
#        if(!is.numeric(res)) return(1)
#
#        return(res)

        owarn <- getOption("warn"); options(warn = -1)
        supp <- union(support(e1), support(e2))
        p1 <- p(e1)(supp)
        p2 <- p(e2)(supp)[p1 != 0]
        p1 <- p1[p1 != 0]
        res <- round(1 - exp(min(log(p2) - log(p1))), 2)
        options(warn = owarn)


        return(list(e1 = e1, e2 = e2, size.of.contamination = res))
    })
