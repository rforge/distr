###############################################################################
## confint methods
###############################################################################

#setMethod("confint", signature(object="ANY"),
#        function(object, parm, level = 0.95, ...) {
#        if(hasArg(parm))
#           stats::confint(object = object, parm = parm, level = level, ...)
#        else
#           stats::confint(object = object, level = level, ...)
#})

setMethod("confint", signature(object="Estimate"),
          function(object, level = 0.95) {
   objN <- paste(deparse(substitute(object)),sep="",collapse="")

   if(is.null(object@asvar))
      { cat(gettextf("Slot 'asvar' of object %s has not (yet) been filled.\n"),
            objN)
        return(NULL) }

   ldx <- length(object@estimate)
   if(is.null(object@nuis.idx))
      idx <- 1:ldx
   else
      idx <- (-object@nuis.idx)

    sd0 <- sqrt(diag(object@asvar)[idx]/object@samplesize)
    names(sd0) <- names(main(object))

### code borrowed from confint.default from package stats
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(idx), 2), dimnames = list(names(object@estimate[idx]),
        pct))
    ci[] <- main(object) + sd0 %o% fac
### end of borrowed code
    new("Confint", type = gettext("asymptotic (CLT-based)"),
                   estimate.call = object@estimate.call,
                   name.estimate = object@name,
                   trafo.estimate = object@trafo,
                   nuisance.estimate = nuisance(object),
                   confint = ci)
})

setMethod("confint", signature(object="Confint"),
           function(object) object@confint)
setMethod("estimate.call", signature(object="Confint"),
           function(object) object@estimate.call)
setMethod("name.estimate", signature(object="Confint"),
           function(object) object@name.estimate)
setMethod("nuisance.estimate", signature(object="Confint"),
           function(object) object@nuisance.estimate)
setMethod("trafo.estimate", signature(object="Confint"),
           function(object) object@trafo.estimate)
setMethod("type", signature(object="Confint"),
           function(object) object@type)

