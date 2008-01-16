.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE)
}

.onAttach <- function(library, pkg){
    unlockBinding(".distrModOptions", asNamespace("distrMod"))
    #msga <- gettext("\n")
    #msgb <- gettext("")
    buildStartupMessage(pkg = "distrMod", #msga, msgb, 
                        library = library, packageHelp = TRUE
        #                    MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
        #, VIGNETTE = gettext("Package \"distrDoc\" provides a vignette to this package as well as\nto several related packages; try vignette(\"distr\").")
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

################################
##
## space classes 
##
################################


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

### from Matthias' thesis / ROptEst
## Parameter of a parametric family of probability measures
setClass("ParamFamParameter",
            representation(main = "numeric",
                           nuisance = "OptionalNumeric",
                           trafo = "matrix"),
            prototype(name = "parameter of a parametric family of probability measures",
                      main = numeric(0), nuisance = NULL, trafo = new("matrix")),
            contains = "Parameter",
            validity = function(object){
                dimension <- length(object@main) + length(object@nuisance)
                if(ncol(object@trafo) != dimension)
                    stop("invalid transformation:\n",
                         "number of columns of 'trafo' not equal to ",
                         "dimension of the parameter")
                if(nrow(object@trafo) > dimension)
                    stop("invalid transformation:\n",
                         "number of rows of 'trafo' larger than ",
                         "dimension of the parameter")
                if(any(!is.finite(object@trafo)))
                    stop("infinite or missing values in 'trafo'")
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
                           modifyParam = "function"    ### <- new !!! (not in thesis!)
                           ),
            prototype(name = "parametric family of probability measures",
                      distribution = new("Norm"),
                      distrSymm = new("NoSymmetry"),
                      modifyParam = function(theta){ Norm(mean=theta) }, ### <- new !!! (not in thesis!)
                      props = character(0),
                      param = new("ParamFamParameter", main = 0, trafo = as.matrix(1))),
            contains = "ProbFamily")
## end Matthias' thesis
###############################################
