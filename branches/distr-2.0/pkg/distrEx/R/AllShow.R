setMethod("show", "MultivariateDistribution",
    function(object){
        cat(gettextf("Distribution object of class: %s\n", class(object)[1]))
        parameter <- param(object)
        Names <- slotNames(parameter)
        if(length(Names) > 1){
          for(i in Names[Names != "name"])
            cat(i, ": ", slot(parameter, i), "\n")
        }
    })
setMethod("show", "EuclCondition",
    function(object){
        cat(gettextf("name:\t%s\n", object@name))
        cat(gettextf("Range:\t%s with dimension %s\n", object@Range@name, object@Range@dimension))
    })
setMethod("show", "LMParameter",
    function(object){
        cat(gettextf("name:\t%s\n", object@name))
        cat(gettextf("theta:\t%s\n", object@theta))
        cat(gettextf("intercept:\t%s\n", object@intercept))
        cat(gettextf("scale:\t%s\n", object@scale))
    })
setMethod("show", "UnivariateCondDistribution",
    function(object){
        cat(gettextf("Distribution object of class: %s\n", class(object)[1]))
        parameter <- param(object)
        Names <- slotNames(parameter)
        if(length(Names) > 1){
          for(i in Names[Names != "name"])
            cat(i, ": ", slot(parameter, i), "\n")
        }
        cat(gettext("## cond:\n"))
        show(object@cond)
    })
