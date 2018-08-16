# Gauﬂ-Legendre abscissas and weights
# cf. for example Numerical Recipies in C (1992), p. 152

#implementation in S:
.GLawOld <- function(n){                                                          
    if(n %% 2 == 1) stop("n has to be an even number")

    m <- (n + 1)/2
    A <- numeric(n)
    W <- numeric(n)
    for(i in 1:floor(m)){
        z <- cos(pi*(i - 0.25)/(n + 0.5))
        repeat{
            p1 <- 1
            p2 <- 0
            for(j in 1:n){
                p3 <- p2
                p2 <- p1
                p1 <- ((2*j - 1)*z*p2 - (j-1)*p3)/j
            }
            pp <- n*(z*p1 - p2)/(z^2 - 1)
            z1 <- z
            z <- z - p1/pp
            if(abs(z - z1) < .Machine$double.eps) break
        }
        A[i] <- -z
        A[n + 1 - i] <- z
        W[i] <- 2/((1 - z^2)*pp^2)
        W[n + 1 - i] <- W[i]
    }
    cbind(A, W)
}

## new: interface to C P.R. 01-04-06

.GLaw <- function(n){
    if(n %% 2 == 1) stop("n has to be an even number")

    A <- numeric(n)
    W <- numeric(n)

#    mm<-dyn.load("G:/rtest/GLaw.dll")
    erg<-.C(C_gauleg,n = as.integer(n),eps = as.double(.Machine$double.eps),
             A = as.double(A),W = as.double(W)) #, PACKAGE = "distrEx")
              ### PACKAGE ARGUMENT added P.R. 270507
              #### removed again 200417 /  used registered symbol instead
#    dyn.unload("G:/rtest/GLaw.dll")
#
# P.R. 20140810: .Call interface instead of .C interface
#
#   erg0 <- .Call("Gauleg", n, eps, PACKAGE="distrEx")
#   erg <- matrix(erg0,n,2); colnames(erg) <- c("A","W")
#
    cbind(A=erg$A, W=erg$W)         
}


if(FALSE){
#   code to produce the AW values stored in the namespace of distrEx
##

## timing code borrowed from base::system.time

    ppt <- function(y) {
        if (!is.na(y[4L]))
            y[1L] <- y[1L] + y[4L]
        if (!is.na(y[5L]))
            y[2L] <- y[2L] + y[5L]
        paste(formatC(y[1L:3L]), collapse = " ")
    }



todo <- c(50, 100, 400, 500, 800, 1000, 4000, 5000, 8000, 10000, 40000, 50000, 80000, 100000)
l <- length(todo)
nE <- new.env()
svncheckout <- "C:/rtest/distr"
pkg <- file.path(svncheckout, "branches/distr-2.8/pkg/distrEx")
sysdataFilename <- file.path(pkg, "R/sysdata.rda")
load(sysdataFilename,envir=nE)

gc()
starttime <- proc.time()
on.exit(message("Timing stopped at: ", ppt(proc.time() - starttime)))

lasttime <- starttime
for(gridsize.i in seq(todo)){
   cat("Gridpoint i =", gridsize.i, ", order = ", todo[gridsize.i],", time needed: ")
   res <- distrEx:::.GLaw(todo[gridsize.i])
   newtime <- proc.time()
   timN <- structure(newtime - lasttime, class = "proc_time")
   lasttime <- newtime
   cat(paste(round(timN,3)), "\n")
   nam <- paste(".AW",as.character(todo[gridsize.i]), sep = ".")
   assign(x=nam, value=res, envir=nE)
}

   timN <- structure(proc.time() - starttime, class = "proc_time")
   cat("Time altogether:", paste(round(timN,3)), "\n")

rm(".AW.100000", envir=nE)
what <- ls(all=TRUE, env=nE)
for(item in what) {cat(item, ":\n");print(object.size(get(item, envir=nE)))}
on.exit()

save(list=what,file=sysdataFilename,envir=nE)
rm(nE)
}

GLIntegrate <- function(f, lower, upper, order = 500, ...){
    if(order %in% c(50, 100, 400, 500, 800, 1000, 4000, 5000, 8000, 10000,
                    40000, 50000, 80000, 100000))
        AW <- getFromNamespace(paste(".AW", as.character(order), 
                                     sep = "."), ns = "distrEx")
    else
        AW <- .GLaw(order)

    # transformation to [lower, upper]
    xl <- (upper - lower)/2
    W <- xl*AW[,2]
    A <- xl*AW[,1] + (lower + upper)/2

    res <- W*c(f(A, ...))
    sum(res)
}

distrExIntegrate <- function(f, lower, upper, subdivisions = 100, 
                             rel.tol = .Machine$double.eps^0.25, 
                             abs.tol = rel.tol, stop.on.error = TRUE, 
                             distr, order = .distrExOptions$GLIntegrateOrder, 
                             ..., diagnostic = FALSE){
    mc <- match.call()
    time <- proc.time()

    ## taken from base::system.time
    ppt <- function(y) {
        if (!is.na(y[4L]))
            y[1L] <- y[1L] + y[4L]
        if (!is.na(y[5L]))
            y[2L] <- y[2L] + y[5L]
        paste(formatC(y[1L:3L]), collapse = " ")
    }

    dots <- list(...)
    dotsFun <- .filterFunargs(dots,f)
    funwD <- function(x) do.call(f,c(list(x), dotsFun))

    on.exit(message("Timing stopped at: ", ppt(proc.time() -
        time)))
    dotsInt <- if(length(names(dots))) dots[names(dots)%in% names(formals(integrate))] else NULL
    res <- try(do.call(integrate, c(list(funwD, lower = lower, upper = upper, rel.tol = rel.tol,
                  abs.tol = abs.tol, stop.on.error = stop.on.error), dotsInt)),
                  silent = TRUE)

    # if integrate fails => Gauﬂ-Legendre integration
    if(!is(res,"try-error")){
       val <- res$value
       if(diagnostic){
          diagn <- list(call = mc, method = "integrate",
                        args = c(list(lower=lower, upper = upper, rel.tol = rel.tol,
                                      abs.tol = abs.tol,
                                      stop.on.error = stop.on.error),list(...)),
                       result = res)
          res <- val
          attr(res,"diagnostic") <- diagn
       }else res <- val
    }else{
        Zi <- 1
        if(lower >= upper){
            lo <- lower
            lower <- upper
            upper <- lo
            Zi <- -1
        }
        if(!missing(distr)){
            q.lDots <- NULL
            if(length(names(dots))) {
               q.lDots <- dots[names(dots) %in% names(formals(q.l(distr)))]
               q.lDots[["p"]] <- q.lDots[["lower.tail"]] <- NULL
            }
        }

        if(!is.finite(lower))
            if(missing(distr)) stop(res)
        else{
            lower <- do.call(q.l(distr),
                       c(list(.distrExOptions$GLIntegrateTruncQuantile),q.lDots))
#            value <- c(...)
#            if(is(distr, "UnivariateCondDistribution"))
#                lower <- q.l(distr)(.distrExOptions$GLIntegrateTruncQuantile,
#                                   cond = value$cond)
#            else
#                lower <- q.l(distr)(.distrExOptions$GLIntegrateTruncQuantile)
        }
        if(!is.finite(upper))
            if(missing(distr)) stop(res)
        else{
           q.lArgs <-  if("lower.tail" %in% names(formals(distr@q)))
               list(p=.distrExOptions$GLIntegrateTruncQuantile, lower.tail=FALSE) else
               list(p=1-.distrExOptions$GLIntegrateTruncQuantile)
           q.lArgs <- c(q.lArgs, q.lDots)
           upper <- do.call(q.l(distr),q.lArgs)
#            value <- c(...)
#            if(is(distr, "UnivariateCondDistribution")){
#                if("lower.tail" %in% names(formals(distr@q)))
#                  upper <- q.l(distr)(.distrExOptions$GLIntegrateTruncQuantile,
#                                      cond = value$cond, lower.tail = FALSE)
#                else
#                  upper <- q.l(distr)(1 - .distrExOptions$GLIntegrateTruncQuantile,
#                                      cond = value$cond)
#            }else{
#                if("lower.tail" %in% names(formals(distr@q)))
#                  upper <- q.l(distr)(.distrExOptions$GLIntegrateTruncQuantile,
#                                     lower.tail = FALSE)
#                else
#                  upper <- q.l(distr)(1 - .distrExOptions$GLIntegrateTruncQuantile)
#            }
        }
        dotsGLInt  <- NULL
        if(length(names(dots))) dotsGLInt <- dots[names(dots)%in% names(formals(GLIntegrate))]
        res <- Zi* do.call(GLIntegrate,c(list(f = funwD, lower = lower, upper = upper,
                              order = order),dotsGLInt))
       if(diagnostic){
          diagn <- list(call = mc, method = "GLIntegrate",
                        args = c(list(lower=lower, upper=upper, order=order),
                               list(...)),
                        result = res,
                        distrExOptions = .distrExOptions)
        }
    }

    new.time <- proc.time()
    on.exit()
    if(diagnostic){
      diagn$time <- structure(new.time - time, class = "proc_time")
      attr(res,"diagnostic") <- diagn
    }
    return(res)
}
