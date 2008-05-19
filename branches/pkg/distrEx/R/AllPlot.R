# plot
setMethod("plot", "MultivariateDistribution", 
    function(x,y=NULL,...){ 
        warning("'plot' not yet implemented for objects",
                " of class ", class(x))
    })

setMethod("plot", "UnivariateCondDistribution", 
    function(x,y=NULL,...){ 
        warning("'plot' not yet implemented for objects",
                " of class ", class(x))
    })


