setClass("NumFunction", representation = list(fun = "function"))

NumFunction <- function(f) new("NumFunction", fun = f)

square <- function(x) x^2
NF <- NumFunction(square)

setMethod("Math", 
          "NumFunction",
          function(x){
            nfun  <- function(n) callGeneric(x@fun(n)) 
            tmp <- function(n) nfun(n)
            NumFunction(tmp)
            })

sinNF <- sin(NF)
sinNF@fun(sqrt(pi/2))

setMethod("Math", 
          "NumFunction",
          function(x){
            nfun  <- function(n) callGeneric(x@fun(n)) 
            NumFunction(nfun)
          })

sinNF <- sin(NF)
sinNF@fun(sqrt(pi/2))

                                        
