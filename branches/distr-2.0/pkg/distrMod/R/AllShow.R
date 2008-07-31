### from Matthias' thesis / ROptEst
setMethod("show", "ParamFamParameter", 
    function(object){
        cat(gettextf("An object of class \"%s\"\n", class(object)))
        cat(gettextf("name:\t%s\n", object@name))
        if(length(object@main) > 0){
            if(length(object@main) > 1){
                if(is.null(names(object@main)))
                    cat(paste(gettextf("element %s of main:\t%s\n", 1:length(object@main), object@main), collapse = ""))
                else{
                    cat(paste(gettextf("%s:\t%s\n", names(object@main), object@main), collapse = ""))
                }
            }else{
                if(is.null(names(object@main)))
                    cat(gettextf("main:\t%s\n", object@main))
                else{
                    cat(gettextf("%s:\t%s\n", names(object@main), object@main))
                }
            }
        }
        if(!is.null(object@nuisance)){
            if(length(object@main) > 1){
                if(is.null(names(object@nuisance)))
                    cat(paste(gettextf("element %s of nuisance:\t%s\n", 1:length(object@nuisance), object@nuisance), collapse = ""))
                else{
                    cat(gettext("nuisance:\n"))
                    cat(paste(gettextf("\t%s:\t%s\n", names(object@nuisance), object@nuisance), collapse = ""))
                }
            }else{
                if(is.null(names(object@nuisance)))
                    cat(gettextf("nuisance:\t%s\n", object@nuisance))
                else{
                    cat(gettext("nuisance:\n"))
                    cat(gettextf("\t%s:\t%s\n", names(object@nuisance), object@nuisance))
                }
            }
        }
        if(!identical(all.equal(object@trafo, diag(length(object)), 
                            tolerance = .Machine$double.eps^0.5), TRUE)){
            cat(gettext("trafo:\n"))
            print(object@trafo)
        }
    })
setMethod("show", "Symmetry", 
    function(object){ 
        cat(gettextf("type of symmetry:\t%s\n", object@type))
        if(!is.null(object@SymmCenter))
            cat(gettext("center of symmetry:\n"))
            print(object@SymmCenter)
    })
setMethod("show", "ParamFamily", 
    function(object){
        cat(gettextf("An object of class \"%s\"\n", class(object)))
        cat(gettextf("### name:\t%s\n", object@name))
        cat(gettext("\n### distribution:\t"))
        print(object@distribution)
        cat(gettext("\n### param:\t"))
        show(object@param)
        if(length(object@props) != 0){
            cat(gettext("\n### props:\n"))
            show(object@props)
        }
    })
setMethod("show", "RiskType", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("risk type:\t", object@type, "\n")
    })
setMethod("show", "asUnOvShoot", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("risk type:\t", object@type, "\n")
        cat("width:\t", object@width, "\n")
    })
setMethod("show", "asHampel", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("risk type:\t", object@type, "\n")
        cat("bound:\t", object@bound, "\n")
    })
setMethod("show", "fiUnOvShoot", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("risk type:\t", object@type, "\n")
        cat("width:\t", object@width, "\n")
    })
setMethod("show", "fiHampel", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("risk type:\t", object@type, "\n")
        cat("bound:\t", object@bound, "\n")
    })

setMethod("show", "Estimate", 
    function(object){
        digits <- getOption("digits")
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        if(length(object@samplesize) > 0)
            cat(gettextf("samplesize:   %d\n",object@samplesize))
        if(!is.null(object@asvar)){
           cat("estimate:\n")
           l.x <- length(object@estimate)
           idx <- 1:l.x
           if(!is.null(object@nuis.idx))
              idx <- (-object@nuis.idx)
           sd0 <- sqrt(diag(object@asvar)[idx]/object@samplesize)
          
        ### code borrowed from print.fitdistr in  package MASS by B.D. Ripley
           ans <- format(rbind(main(object), sd0), digits=digits)
           ans[1, ] <- sapply(ans[1, ], function(x) paste("", x))
           ans[2, ] <- sapply(ans[2, ], function(x) paste("(", x, ")", sep=""))
           ## only used for digits
           dn <- dimnames(ans)
           dn[[1]] <- rep("", 2)
           dn[[2]] <- paste(substring("      ", 1, 
                             (nchar(ans[2,]) - nchar(dn[[2]])) %/% 2), dn[[2]])
           dn[[2]] <- paste(dn[[2]], substring("      ", 1, 
                             (nchar(ans[2,]) - nchar(dn[[2]])) %/% 2))
           dimnames(ans) <- dn
           print(ans, quote = FALSE)
       ### end of borrowed code  

           cat("asymptotic (co)variance:\n")
           asvar <- object@asvar
           rownames(asvar) <- colnames(asvar) <- names(object@estimate)
           if(!is.null(object@nuis.idx))
               asvar <- c(asvar[-idx], asvar[idx])  
           print(asvar, digits = digits)
        }else{
           cat("estimate:\n")
           print(main(object), digits = digits)
        } 
        if(!is.null(object@nuis.idx)){
           cat("nuisance parameter:\n")
           print(nuisance(object), digits = digits)        
           }
        if(nrow(object@Infos) > 0){
          cat("Infos:\n")
          print(object@Infos)
        }
    })

setMethod("print", "Estimate", 
    function(x, digits = getOption("digits")){
        oldDigits <- getOption("digits")
        options("digits" = digits)
        show(object = x)
        options("digits" = oldDigits)
        })