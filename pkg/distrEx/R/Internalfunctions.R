## for internal use only ...

## discretize distributions
.discretizeDistr <- function(D, x, lower, upper, n){
#    print(list(D, x, lower, upper, n))
    y <- seq(from = lower, to = upper, length = n)
    x <- x[x<=upper & x>=lower]
    supp <- y[-1]-(y[2]-y[1])/2
    prob <- diff(p(D)(y))
    prob[1] <- prob[1] + p(D)(y[1])
    prob[n-1] <- prob[n-1] + p(D)(y[n], lower = FALSE)
    ind <- sapply(x, function(x, y) which.min(abs(x-y)), y)
    ind <- ind[ind<n]
    tab <- table(ind)
    if(any(tab > 1)){
        tab.names <- as.integer(names(tab))
        add.supp <- numeric(length(x) - length(tab))
        add.prob <- add.supp
        j <- 0
        for(i in tab.names){
            nr <- sum(ind == i)
            if(nr > 0)  
              {
               if(nr == 1){
                     supp[i] <- x[which(ind == i)]
               }else{
                     j.alt <- j
                     j <- j + nr
                     supp[i] <- NA
                     add.supp[(j.alt+1):j] <- x[which(ind == i)]
                     add.prob[(j.alt+1):j] <- prob[i]/nr
                     prob[i] <- NA
                    }
              }      
        }
        supp <- c(supp, add.supp)
        supp <- supp[!is.na(supp)]
        prob <- c(prob, add.prob)
        prob <- prob[!is.na(prob)]
    }else{
        supp[ind] <- x
    }

    o.warn <- getOption("warn")
    options(warn = -1)
    on.exit(options(warn=o.warn))
    DD <- DiscreteDistribution(supp = supp, prob = prob)

    return(DD)
}

## smooth distributions
.smoothDistr <- function(D, h){
    return(D + Norm(0, h))
}

## compute empirical density (probability function)
.empiricalDensity <- function(Dat){
    table(Dat)/length(Dat)
}

## compute empirical distribution
.empiricalDistribution <- function(Dat){
    den <- .empiricalDensity(Dat)
    return(DiscreteDistribution(supp = as.numeric(names(den)), prob = den))
}

## modify distributions to avoid trivial distances
.asis.smooth.discretize.distance <- function(x, Distribution, asis.smooth.discretize, 
        n.discr, low.discr, up.discr, h.smooth, distance, ..., diagnostic = FALSE){
    ASD <- pmatch(asis.smooth.discretize, c("asis", "smooth", "discretize"), nomatch = 3)
    dots <- list(...)
    dotsn <- names(dots)
    dotsD <- list()
    formalsD <- names(formals(distance))
    for(item in dotsn) if(item %in% formalsD) dotsD[[item]] <- dots[[item]]
    dotsD <- c(dotsD,dots[!dotsn %in% formalsD])
    if (ASD == 1){
       res <- do.call(distance,c(list(e1=x, e2=Distribution),dotsD))
       return(res)
    }
    if (ASD == 2){
        Dx <- .smoothDistr(.empiricalDistribution(x), h = h.smooth)
        distArgs <- c(list(e1=Dx,e2=Distribution),dotsD,list(diagnostic=diagnostic))
        return(do.call(distance,distArgs))
    }
    if (ASD == 3){
        DD <- .discretizeDistr(D = Distribution, x = x, n = n.discr, lower = low.discr, upper = up.discr)
        res <- do.call(distance,c(list(e1=x, e2=DD),dotsD))
       return(res)
    }
}
