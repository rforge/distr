.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE)
## are the following calls to require necessary?
    require("distr", character = TRUE, quietly = TRUE)
    require("distrEx", character = TRUE, quietly = TRUE)
    require("RandVar", character = TRUE, quietly = TRUE)
}

.onAttach <- function(library, pkg){
    unlockBinding(".distrModOptions", asNamespace("distrMod"))
    #msga <- gettext("\n")
    #msgb <- gettext("")
    buildStartupMessage(pkg = "distrMod", #msga, msgb, 
                        library = library, packageHelp = TRUE,
        #                    MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
        VIGNETTE = gettext("Package \"distrDoc\" provides a vignette to this package as well as\nto several related packages; try vignette(\"distr\").")
        )
    invisible()
}

################################
##
## Optional..-classes
##
################################

### from Matthias' thesis / ROptEst
## optional numeric
setClassUnion("OptionalNumeric", c("numeric", "NULL"))
setClassUnion("MatrixorFunction", c("matrix", "function"))


################################
##
## classes
##
################################
### from Matthias' thesis / ROptEst
## positive definite, symmetric matrices with finite entries
setClass("PosSemDefSymmMatrix", contains = "matrix",
            prototype = prototype(matrix(1)),
            validity = function(object){
                if(nrow(object) != ncol(object))
                    stop("no square matrix")
                if(any(!is.finite(object)))
                    stop("inifinite or missing values in matrix")
                if(!isTRUE(all.equal(object, t(object), .Machine$double.eps^0.5)))
                    stop("matrix is not symmetric")
                if(!all(eigen(object)$values > -100*.Machine$double.eps))
                   stop("matrix is (numerically) not positive semi - definite")
               return(TRUE)
            })

## positive definite, symmetric matrices with finite entries
setClass("PosDefSymmMatrix", contains = "PosSemDefSymmMatrix",
            validity = function(object){
               if(!all(eigen(object)$values > 100*.Machine$double.eps))
                   stop("matrix is (numerically) not positive definite")
               valid <- getValidity(getClass("PosSemDefSymmMatrix"))
               valid(as(object, "PosSemDefSymmMatrix"))
               return(TRUE)
            })

### from Matthias' thesis / ROptEst
## class of symmetries
setClass("Symmetry", representation(type = "character",
                                    SymmCenter = "ANY"),
                     contains = "VIRTUAL")

### from Matthias' thesis / ROptEst
## symmetry of distributions
setClass("DistributionSymmetry", contains = c("Symmetry", "VIRTUAL"))

## no symmetry
setClass("NoSymmetry", contains = "DistributionSymmetry", 
            prototype = prototype(type = "non-symmetric distribution",
                                  SymmCenter = NULL))

## elliptical symmetry
setClass("EllipticalSymmetry", contains = "DistributionSymmetry", 
            prototype = prototype(type = "elliptically symmetric distribution",
                                  SymmCenter = numeric(0)))

## spherical symmetry
setClass("SphericalSymmetry", contains = "EllipticalSymmetry", 
            prototype = prototype(type = "spherically symmetric distribution",
                                  SymmCenter = numeric(0)))

## symmetry of functions
setClass("FunctionSymmetry", contains = c("Symmetry", "VIRTUAL"))

## non-symmetric functions
setClass("NonSymmetric", contains = "FunctionSymmetry", 
            prototype = prototype(type = "non-symmetric function",
                                  SymmCenter = NULL))

## even functions
setClass("EvenSymmetric", contains = "FunctionSymmetry", 
            prototype = prototype(type = "even function",
                                  SymmCenter = numeric(0)))

## odd functions
setClass("OddSymmetric", contains = "FunctionSymmetry", 
            prototype = prototype(type = "odd function",
                                  SymmCenter = numeric(0)))

## list of symmetry types
setClass(Class = "DistrSymmList", 
            prototype = prototype(list(new("NoSymmetry"))), 
            contains = "list",
            validity = function(object){
                nrvalues <- length(object)
                for(i in 1:nrvalues)
                    if(!is(object[[i]], "DistributionSymmetry")) 
                        stop("element ", i, " is no 'DistributionSymmetry'")
                return(TRUE) 
            })

## list of symmetry types
setClass(Class = "FunSymmList", 
            prototype = prototype(list(new("NonSymmetric"))), 
            contains = "list",
            validity = function(object){
                nrvalues <- length(object)
                for(i in 1:nrvalues)
                    if(!is(object[[i]], "FunctionSymmetry")) 
                        stop("element ", i, " is no 'FunctionSymmetry'")
                return(TRUE) 
            })

### from Matthias' thesis / ROptEst
## Parameter of a parametric family of probability measures
setClass("ParamFamParameter",
            representation(main = "numeric",
                           nuisance = "OptionalNumeric",
                           trafo = "MatrixorFunction"),
            prototype(name = "parameter of a parametric family of probability measures",
                      main = numeric(0), nuisance = NULL, trafo = new("matrix")),
            contains = "Parameter",
            validity = function(object){
                if(is.matrix(object@trafo)){
                dimension <- length(object@main) + length(object@nuisance)

                if(is.matrix(object@trafo)){
                    if(ncol(object@trafo) != dimension)
                        stop("invalid transformation:\n",
                             "number of columns of 'trafo' not equal to ",
                             "dimension of the parameter")
                    if(nrow(object@trafo) > dimension)
                        stop("invalid transformation:\n",
                             "number of rows of 'trafo' larger than ",
                             "dimension of the parameter")
                    if(any(!is.finite(object@trafo)))
                        stop("infinite or missing values in 'trafo'")}
                }
                return(TRUE)
            })

### from Matthias' thesis / ROptEst
## family of probability measures
setClass("ProbFamily", representation(name = "character",
                                      distribution = "Distribution",
                                      distrSymm = "DistributionSymmetry",
                                      props = "character"),
                       contains = "VIRTUAL")

### from Matthias' thesis / ROptEst
## parametric family of probability measures
setClass("ParamFamily",
            representation(param = "ParamFamParameter",
                           modifyParam = "function"
                           ### <- new !!! (not in thesis!)
                           ### a function with argument theta
                           ###  returning distribution P_theta    
                           ),
            prototype(name = "parametric family of probability measures",
                      distribution = new("Norm"),
                      distrSymm = new("NoSymmetry"),
                      modifyParam = function(theta){ Norm(mean=theta) }, ### <- new !!! (not in thesis!)
                      props = character(0),
                      param = new("ParamFamParameter", main = 0, trafo = matrix(1))),
            contains = "ProbFamily")

### from Matthias' thesis / ROptEst
## L2-differentiable parametric family of probability measures
setClass("L2ParamFamily",
            representation(L2deriv = "EuclRandVarList",
                           L2deriv.fct = "function", ## new: a function in theta which produces L2deriv
                           L2derivSymm = "FunSymmList",
                           L2derivDistr = "DistrList",
                           L2derivDistrSymm = "DistrSymmList",
                           FisherInfo = "PosSemDefSymmMatrix",
                           FisherInfo.fct = "function" ## new: a function in theta which produces FisherInfo
                           ), 
            prototype(name = "L_2 differentiable parametric family of probability measures",
                      distribution = new("Norm"),
                      distrSymm = new("NoSymmetry"),
                      modifyParam = function(theta){ Norm(mean=theta) }, ### <- new !!! (not in thesis!)
                      param = new("ParamFamParameter", main = 0, trafo = matrix(1)),
                      props = character(0),
                      L2deriv.fct = function(theta) {f <- function(x) {x-theta} 
                                                     return(f)},
                      L2deriv = EuclRandVarList(RealRandVariable(Map = list(function(x)x), 
                                                                 Domain = Reals())),
                      L2derivSymm = new("FunSymmList"),
                      L2derivDistr = UnivarDistrList(new("Norm")),
                      L2derivDistrSymm = new("DistrSymmList"),
                      FisherInfo.fct = function(theta)return(1),
                      FisherInfo = new("PosDefSymmMatrix")),
            contains = "ParamFamily", 
            validity = function(object){
                if(is(object@distribution, "UnivariateCondDistribution"))
                    stop("conditional distributions are not allowed in slot 'distribution'")

                if(!is(object@distrSymm, "NoSymmetry")){
                    if(!is(object@distrSymm@SymmCenter, "numeric"))
                        stop("slot 'SymmCenter' of 'distrSymm' has to be of class 'numeric'")
                    if(length(object@distrSymm@SymmCenter) != dimension(img(object@distribution)))
                        stop("slot 'SymmCenter' of 'distrSymm' has wrong dimension")
                }

                if(!is(object@FisherInfo, "PosDefSymmMatrix")) 
                    warning("Fisher information is singular; not all parameter directions can be inferred.")    

                dims <- length(object@param)
                if(ncol(object@FisherInfo) != dims)
                    stop(paste("dimension of 'FisherInfo' should be", dims))
                nrvalues <- numberOfMaps(object@L2deriv)
                if(nrvalues != length(object@L2derivSymm))
                    stop("number of Maps of 'L2deriv' != length of 'L2derivSymm'")
                if(nrvalues != length(object@L2derivDistr))
                    stop("number of Maps of 'L2deriv' != length of 'L2derivDistr'")
                if(nrvalues != length(object@L2derivDistrSymm))
                    stop("number of Maps of 'L2deriv' != length of 'L2derivDistrSymm'")
                if(dimension(Domain(object@L2deriv[[1]])) != dimension(img(object@distribution)))
                    stop("dimension of 'Domain' of 'L2deriv' != dimension of 'img' of 'distribution'")
                if(dimension(object@L2deriv) != dims)
                    stop("dimension of 'L2deriv' != dimension of parameters")

                return(TRUE) 
            })

## L2-differentiable parametric family of probability measures generated by a group
setClass("L2GroupParamFamily",
            representation(LogDeriv = "function"), 
            prototype(name = "L_2 differentiable parametric group family",
                      Logderiv = function(x)x),
            contains = "L2ParamFamily")

## L2-differentiable (univariate) location family
setClass("L2LocationFamily",
            contains = "L2GroupParamFamily")

## L2-differentiable (univariate) location scale family
setClass("L2ScaleFamily",
            contains = "L2GroupParamFamily")

## L2-differentiable (univariate) location and scale family
setClass("L2LocationScaleFamily",
            contains = "L2GroupParamFamily")

################################################################################
## Bias Classes
################################################################################

### from session 10-01-08 : class Bias type
setClass("BiasType", representation(name = "character"),
          contains = "VIRTUAL")

setClass("symmetricBias", prototype = prototype(name = "symmetric bias"),
          contains = "BiasType")

setClass("onesidedBias", representation(sign = "numeric"),
          prototype = prototype(name = "positive bias", sign = 1),
          contains = "BiasType")

setClass("asymmetricBias", 
          representation(nu = "numeric"), ### weights acc. to paper
          prototype = prototype(name = "asymmetric bias", nu = c(1,1)),
          contains = "BiasType")

################################################################################
## Risk Classes
################################################################################

## risks (e.g., risk of estimator)
setClass("RiskType", representation(type = "character"), 
          contains = "VIRTUAL")
## asymptotic risk
setClass("asRisk", contains = c("RiskType", "VIRTUAL"))
## asymptotic covariance
setClass("asCov", contains = "asRisk", 
            prototype = prototype(type = "asymptotic covariance"))
## trace of asymptotic covariance
setClass("trAsCov", contains = "asRisk", 
            prototype = prototype(type = "trace of asymptotic covariance"))

## asymptotic risk with bias

setClass("asRiskwithBias", representation(biastype = "BiasType"),
          prototype = prototype(type = "asymptotic risk with bias",
                             biastype = new("symmetricBias")),
          contains = c("asRisk"))

## asymptotic Hampel risk
setClass("asHampel", representation(bound = "numeric"), 
            prototype = prototype(bound = Inf, 
                             type = "trace of asymptotic covariance for given bias bound"),
            contains = "asRiskwithBias", 
            validity = function(object){
                if(any(object@bound <= 0))
                    stop("'bound' has to be positive")
                else TRUE
            })
## asymptotic bias
setClass("asBias", representation(biastype = "BiasType"),
            contains = "asRiskwithBias", 
            prototype = prototype(type = "asymptotic bias"))

## convex asymptotic risk
setClass("asGRisk", contains = "asRiskwithBias") 
## asymptotic mean square error
setClass("asMSE", contains = "asGRisk",
            prototype = prototype(type = "asymptotic mean square error"))
## asymptotic under-/overshoot probability
setClass("asUnOvShoot", representation(width = "numeric"), 
            prototype = prototype(type = "asymptotic under-/overshoot probability"),
            contains = "asGRisk",
            validity = function(object){
                if(length(object@width) != 1)
                    stop("length of 'width' has to be 1")
                if(any(object@width <= 0))
                    stop("'width' has to be positive")
                else TRUE
            })
## finite-sample risk
setClass("fiRisk", contains = c("RiskType", "VIRTUAL"))
## finite-sample covariance
setClass("fiCov", contains = "fiRisk", 
            prototype = prototype(type = "finite-sample covariance"))
## trace of finite-sample covariance
setClass("trFiCov", contains = "fiRisk", 
            prototype = prototype(type = "trace of finite-sample covariance"))
## finite-sample Hampel risk
setClass("fiHampel", representation(bound = "numeric"),
            prototype = prototype(bound = Inf, 
                             type = "finite-sample variance for given bias bound"),
            contains = "fiRisk", 
            validity = function(object){
                if(any(object@bound <= 0))
                    stop("'bound' has to be positive")
                else TRUE
            })
## finite-sample mean square error
setClass("fiMSE", contains = "fiRisk", 
            prototype = prototype(type = "finite-sample mean square error"))
## finite-sample bias
setClass("fiBias", contains = "fiRisk", 
            prototype = prototype(type = "finite-sample bias"))
## finite-sample under-/overshoot probability
setClass("fiUnOvShoot", representation(width = "numeric"), 
            prototype = prototype(type = "finite-sample under-/overshoot probability"),
            contains = "fiRisk",
            validity = function(object){
                if(length(object@width) != 1)
                    stop("length of 'width' has to be 1")
                if(any(object@width <= 0))
                    stop("'width' has to be positive")
                else TRUE
            })
## end Matthias' thesis
###############################################

setClass("asSemivar", 
          contains = "asGRisk",
          prototype = prototype(type = "asymptotic Semivariance",
          biastype =  new("onesidedBias")))
