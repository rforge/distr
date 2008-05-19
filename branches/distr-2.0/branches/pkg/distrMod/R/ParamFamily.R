### from Matthias' thesis / ROptEst
### extended: new slot/argument modifyParam
ParamFamily <- function(name, distribution = Norm(), distrSymm,
                        modifyParam, main = 0, nuisance, trafo, param,
                        props = character(0)){
    if(missing(name))
        name <- "parametric family of probability measures"
    if(missing(distrSymm)) distrSymm <- NoSymmetry()
    if(missing(param))
        param <- ParamFamParameter(name = paste("parameter of", name),
                        main = main, nuisance = nuisance, trafo = trafo)
    if(missing(modifyParam))
       stop(cat(paste("Please enter a function(theta) with value a new instance of",
                      "slot distribution with corresponding parameter value theta.",
                      "example (normal location) function(theta){ Norm(mean=theta)}\n", collapse="",sep="")))
    PF <- new("ParamFamily")
    PF@name <- name
    PF@distribution <- distribution
    PF@distrSymm <- distrSymm
    PF@param <- param
    PF@props <- props
    PF@modifyParam <- modifyParam

    return(PF)
}


## access methods
setMethod("param", "ParamFamily", function(object) object@param)
setMethod("modifyParam", "ParamFamily", function(object) object@modifyParam)

## wrapped access methods
setMethod("main", "ParamFamily", function(object) main(param(object)))
setMethod("nuisance", "ParamFamily", function(object) nuisance(param(object)))
setMethod("trafo", "ParamFamily", function(object) trafo(param(object)))

## replace methods
#setReplaceMethod("param", "ParamFamily", 
#    function(object, value){ object@param <- value; object })
