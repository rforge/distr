### from Matthias' thesis / ROptEst
setMethod("plot", "ParamFamily", 
    function(x,y=NULL,...){ 
        e1 <- x@distribution
        if(!is(e1, "UnivariateDistribution")) stop("not yet implemented")

        plot(e1) 
    })
setMethod("plot", "L2ParamFamily",
    function(x,y=NULL,...){

        dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."

        e1 <- x@distribution
        if(!is(e1, "UnivariateDistribution")) stop("not yet implemented")

        do.call(plot, c(list(e1),dots))

        if(is(e1, "AbscontDistribution")){
            lower <- ifelse(is.finite(q(e1)(0)), q(e1)(0), q(e1)(getdistrOption("TruncQuantile")))
            upper <- ifelse(is.finite(q(e1)(1)), q(e1)(1), q(e1)(1 - getdistrOption("TruncQuantile")))
            h <- upper - lower
            x.vec <- seq(from = lower - 0.1*h, to = upper + 0.1*h, length = 1000)
            plty <- "l"
            lty <- "solid"
        }else{
            if(is(e1, "DiscreteDistribution")){ 
                x.vec <- support(e1)
                plty <- "p"
                lty <- "dotted"
            }else{
                x.vec <- r(e1)(1000)
                x.vec <- sort(unique(x.vec))
                plty <- "p"
                lty <- "dotted"
            }
        }

        dots["lty"]  <- NULL
        dots["type"] <- NULL
        dots["xlab"] <- NULL
        dots["ylab"] <- NULL
        
        dims <- length(x@param)
        L2deriv <- as(diag(dims) %*% x@L2deriv, "EuclRandVariable")

        w0 <- options("warn")
        options(warn = -1)
        opar <- par()
        devNew()
        nrows <- trunc(sqrt(dims))
        ncols <- ceiling(dims/nrows)
        par(mfrow = c(nrows, ncols))

        if(is.null(dots[["cex.main"]])) dots[["cex.main"]] <- 0.8
                
        for(i in 1:dims){
            do.call(plot, args=c(list(x=x.vec, y=sapply(x.vec, L2deriv@Map[[i]]), 
                                 type = plty, lty = lty,
                                 xlab = "x", 
                                 ylab = expression(paste(L[2], " derivative"))),
                                 dots))
            if(is(e1, "DiscreteDistribution")){
                x.vec1 <- seq(from = min(x.vec), to = max(x.vec), length = 1000)
                lines(x.vec1, sapply(x.vec1, L2deriv@Map[[i]]), lty = "dotted")
            }
            if(is.null(x@param@nuisance))
                do.call(title, args = c(list(paste("Component", i, "of L_2 derivative\nof", name(x)[1], 
                            "\nwith main parameter (", paste(round(x@param@main, 3), collapse = ", "), ")")), 
                            dots))
            else
                do.call(title, args = c(list(paste("Component", i, "of L_2 derivative of", name(x)[1], 
                            "\nwith main parameter (", paste(round(x@param@main, 3), collapse = ", "),
                            ")\nand nuisance parameter (", paste(round(x@param@nuisance, 3), collapse = ", "), ")")), 
                            dots))
        }
        par(opar)    
        options(w0)
        invisible()
    })
