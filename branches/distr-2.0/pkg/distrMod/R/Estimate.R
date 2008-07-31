###############################################################################
## Functions and methods for "Estimate" classes
###############################################################################

setMethod("name", "Estimate", function(object) object@name)
setReplaceMethod("name", "Estimate", 
                  function(object, value) {object@name <- value; object})

setMethod("estimate", "Estimate", function(object) object@estimate)

setMethod("Infos", "Estimate", function(object) object@Infos)
setReplaceMethod("Infos", "Estimate", 
    function(object, value){ 
        object@Infos <- value 
        if(!is.character(value))
            stop("'value' is no matrix of characters")
        if(ncol(value)!=2)
            stop("'value' has to be a matrix with two columns")
        object
    })

setMethod("addInfo<-", "Estimate", 
    function(object, value){ 
        object@Infos <- rbind(object@Infos, " " = value) 
        if(length(value)!=2)
            stop("length of 'value' is != 2")
        if(!is.character(value))
            stop("'value' is no vector of characters")
        object 
    })

setMethod("samplesize", "Estimate", function(object) object@samplesize)
setMethod("asvar", "Estimate", function(object) object@asvar)

setMethod("criterion", "MCEstimate", function(object) object@criterion)
setReplaceMethod("criterion", "MCEstimate", 
                  function(object, value) {object@criterion <- value; object})


setMethod("nuisance", "Estimate", function(object) { 
      if(is.null(object@nuis.idx))
         return(NULL)
      else return (object@estimate[object@nuis.idx])    
      })
setMethod("main", "Estimate", function(object) { 
      if(is.null(object@nuis.idx))
         return(object@estimate)
      else return (object@estimate[-object@nuis.idx])    
      })


