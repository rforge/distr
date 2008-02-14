## access method
setMethod("name", "BiasType", function(object) object@name)
setMethod("sign", "onesidedBias", function(x) x@sign)
setMethod("nu", "asymmetricBias", function(object) object@nu)

## generating function
onesidedBias <- function(name = "positive bias", sign = 1){ 
    new("onesidedBias", name = name, sign = sign) 
}

## generating function
asymmetricBias <- function(name = "asymmetric bias", nu = c(1, 1)){ 
    new("symmetricBias", name = name, sign = sign) 
}

## generating function
symmetricBias <- function(){ new("symmetricBias") }
