###############################################################################
## confint methods
###############################################################################

#setMethod("confint", "ANY", function(object, parm, level = 0.95, ...) {
#        if(hasArg(parm))
#           stats::confint(object = object, parm = parm, level = level, ...)
#        else
#           stats::confint(object = object, level = level, ...)
#})

setMethod("confint", "Estimate", function(object, level = 0.95) {
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
    ci
### end of borrowed code
})