setMethod("solve", signature(a = "ANY", b = "ANY"), function(a,b, generalized = TRUE,
          tol = .Machine$double.eps, ...) {if(!generalized) return(base::solve(a,b, tol = tol, ...))
          else if(is(try(return(base::solve(a,b, tol = tol, ...)), silent = TRUE),
                                        "try-error")){
             a.svd <- svd(a)
             d1 <- a.svd$d
             d1.0 <- (d1 < tol) + 0.0
             d1.1 <- 1/pmax(d1, d1.0)
             d <- (1-d1.0) * d1.1
             d <- if (length(d)==1) diag(d) else d
             a.m <- a.svd$v %*% d %*% t(a.svd$u)
             if (!missing(b))
                if(!(length(b)==nrow(a))) stop("non-conformable arguments")
             erg <- if (missing(b)) a.m else a.m%*%b
             return(erg)}
             }
             )

setMethod("solve", signature(a = "PosSemDefSymmMatrix", b = "ANY"), function(a,b, generalized = TRUE, tol = .Machine$double.eps, ...){
          if(!generalized) return(base::solve(a,b, tol = tol, ...))
          else{
            er <- eigen(a)
            d1 <- er$values
            d1.0 <- (d1 < tol) + 0.0
            d1.1 <- 1/pmax(d1, d1.0)
            d <- (1-d1.0) * d1.1
            d <- if (length(d)==1) diag(d) else d
            A <- er$vectors %*% d %*% t(er$vectors)
            if(missing(b)) return(A)
            else return(A%*%b)}
})

setMethod("solve", signature(a = "PosDefSymmMatrix", b = "ANY"), function(a,b, ...){
base::solve(a,b, ...)})

