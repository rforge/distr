###############################################################################
# Semivariance Risk
###############################################################################

## generating function
asSemivar <- function(sign = 1){ new("asSemivar", sign = sign) }

setMethod("sign", "asSemivar", function(x) x@sign)
setReplaceMethod("sign", "asSemivar", function(object, value)
                     {if (abs(trunc(value))!=1) stop("Left value has to be +-1")
                      object@sign <- value; object})

