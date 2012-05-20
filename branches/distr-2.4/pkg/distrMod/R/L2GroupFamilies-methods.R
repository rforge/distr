##################################################################
## L2 Group family methods
##################################################################
setMethod("LogDeriv", signature(object = "L2GroupParamFamily"),
           function(object) object@LogDeriv)

setMethod("locscalename", signature(object = "L2LocationScaleUnion"),
           function(object) object@locscalename)

setMethod("withPosRestr", signature(object = "L2ScaleShapeUnion"),
           function(object) object@param@withPosRestr)

setReplaceMethod("LogDeriv", "L2GroupParamFamily",
    function(object, value){
        object@LogDeriv <- value
        object
    })

setReplaceMethod("locscalename", "L2LocationScaleUnion",
    function(object, value){
        if(length(value)>2||length(value)<1)
           stop("value of slot 'locscalename' must be of length one or two")
        object@locscalename <- value
        object
    })

setReplaceMethod("withPosRestr", "L2ScaleShapeUnion",
    function(object, value){
        if(length(value)!=1)
           stop("value of slot 'withPosRestr' must be of length one")
        param <- object@param
        withPosRestr(param) <- value
        object@param <- param
        object
    })

