## access methods
setMethod("loc", "GumbelParameter", function(object) object@loc)
setMethod("scale", "GumbelParameter", 
    function(x, center = TRUE, scale = TRUE) x@scale)

## replace Methods
setReplaceMethod("loc", "GumbelParameter", 
    function(object, value){ object@loc <- value; object })
setReplaceMethod("scale", "GumbelParameter", 
    function(object, value){ object@scale <- value; object})


## generating function
Gumbel <- function(loc = 0, scale = 1){ new("Gumbel", loc = loc, scale = scale) }

## wrapped access methods
setMethod("loc", "Gumbel", function(object) loc(object@param))
setMethod("scale", "Gumbel", 
    function(x, center = TRUE, scale = TRUE) scale(x@param))

## wrapped replace methods
setMethod("loc<-", "Gumbel", 
    function(object, value){ 
        new("Gumbel", loc = value, scale = scale(object))
    })
setMethod("scale<-", "Gumbel", 
    function(object, value){ 
        if(length(value) != 1 || value <= 0)
            stop("'value' has to be a single positive number")
        new("Gumbel", loc = loc(object), scale = value)
    })
