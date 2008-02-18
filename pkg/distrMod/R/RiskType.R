## access method
setMethod("type", "RiskType", function(object) object@type)

## generating function
asCov <- function(){ new("asCov") }

## generating function
trAsCov <- function(){ new("trAsCov") }

## generating function
asHampel <- function(bound = Inf, biastype = symmetricBias()){ 
   new("asHampel", bound = bound, biastype = biastype) }

## access method
setMethod("bound", "asHampel", function(object) object@bound)

## generating function
asBias <- function(biastype = symmetricBias()){ 
          new("asBias", biastype = biastype) }

## generating function
asMSE <- function(biastype = symmetricBias()){ new("asMSE", biastype = biastype) }

## generating function
asUnOvShoot <- function(width = 1.960, biastype = symmetricBias())
              { new("asUnOvShoot", width = width, 
                     biastype = biastype) }

## access method
setMethod("width", "asUnOvShoot", function(object) object@width)

## generating function
fiCov <- function(){ new("fiCov") }

## generating function
trFiCov <- function(){ new("trFiCov") }

## generating function
fiHampel <- function(bound = Inf){ new("fiHampel", bound = bound) }

## access method
setMethod("bound", "fiHampel", function(object) object@bound)

## generating function
fiMSE <- function(){ new("fiMSE") }

## generating function
fiBias <- function(){ new("fiBias") }

## generating function
fiUnOvShoot <- function(width = 1.960){ new("fiUnOvShoot", width = width) }

## access method
setMethod("width", "fiUnOvShoot", function(object) object@width)


## access method
setMethod("biastype", "asRiskwithBias", function(object) object@biastype)
## replacement method
setReplaceMethod("biastype", "asRiskwithBias", function(object,value) {
                  object@biastype <- value; object})

###############################################################################
# Semivariance Risk
###############################################################################

## generating function
asSemivar <- function(sign = 1){ 
       new("asSemivar", biastype = new("onesidedBias", name = 
       ifelse(sign>0, "positive Bias", "negative Bias"), sign = sign)) }

setMethod("sign", "asSemivar", function(x) x@biastype@sign)
setReplaceMethod("sign", "asSemivar", function(object, value)
                     {if (abs(trunc(value))!=1) stop("Left value has to be +-1")
                      sign(biastype(object)) <- value; 
                      biastype(object)@name <- ifelse(sign>0, "positive Bias", 
                                                      "negative Bias")
                      object})

