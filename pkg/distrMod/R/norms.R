
setMethod("name", signature = "NormType", function(object) object@name)
setReplaceMethod("name", signature = "NormType", function(object,value) 
        {object@name <- value; object})

setMethod("fct", signature = "NormType", function(object) object@fct)
setReplaceMethod("fct", signature = "NormType", function(object,value) 
        {object@fct <- value; object})

setMethod("QuadForm", signature = "QFNorm", function(object) object@QuadForm)
setReplaceMethod("QuadForm", signature = "QFNorm", function(object,value) 
        {object@QuadForm <- value; 
         object@fct <- function(x) QuadFormNorm(x, A=value)
         object})
         
NormType <- function(name = "EuclideanNorm", fct = EuclideanNorm){
             new("NormType", name = name, fct = fct)}         
QFNorm <- function(name = "norm based on quadratic form", 
                   QuadForm = PosSemDefSymmMatrix(matrix(1))){
             force( A0 <- QuadForm)      
             new("QFNorm", name = name, fct = function(x) QuadFormNorm(x, A= A0), 
                  QuadForm = QuadForm)}
InfoNorm <- function(){A <- PosSemDefSymmMatrix(matrix(1))
            new("InfoNorm", name = "Information matrix Norm", 
                 fct = function(x) QuadFormNorm(x, A = A), 
                 QuadForm = A)}
SelfNorm <- function(){A <- PosSemDefSymmMatrix(matrix(1))
            new("SelfNorm", name =  "Information matrix Norm", 
                 fct = function(x) QuadFormNorm(x, A = A), 
                 QuadForm = A)}

setMethod("updateNorm", "NormType", function(normtype, ...) normtype)
setMethod("updateNorm", "InfoNorm", function(normtype, FI, ...)
           {QuadForm(normtype) <- FI; normtype})
setMethod("updateNorm", "SelfNorm", function(normtype, L2, stand, 
                                             cent, clip, Distr, norm, ...)
           {Cv <- .asCovMB(L2, stand, cent, clip, Distr, norm)
            QuadForm(normtype) <- Cv; normtype})                                 