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
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("estimate:\n")
        print(object@estimate)
        if(nrow(object@Infos) > 0){
          cat("Infos:\n")
          print(object@Infos)
        }
    })
