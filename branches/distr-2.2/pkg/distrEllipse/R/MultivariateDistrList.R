
# coerce to "DistrList"
setAs(from = "MultivariateDistribution", to = "MultivarDistrList",
    def = function(from){ new("MultivarDistrList", list(from)) })


# generating function for class 'MultivarDistrList'
MultivarDistrList <- function(...){ 
    new("MultivarDistrList", list(...)) 
}

# coerce to "MultivarDistrList"
setAs(from = "MultivariateDistribution", to = "MultivarDistrList",
    def = function(from){ new("MultivarDistrList", list(from)) })

setMethod("dim", "MultivarDistrList", function(x) x[[1]]@img@dimension)
