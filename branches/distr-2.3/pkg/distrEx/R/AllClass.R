.isEqual01 <- distr:::.isEqual01 ## for faster access due to local caching in package namespace

.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE)
}


.onAttach <- function(library, pkg)
{
unlockBinding(".distrExOptions", asNamespace("distrEx"))
msga <- gettext("Note: Packages \"e1071\", \"moments\", \"fBasics\" should be attached ")
msgb <- gettext("/before/ package \"distrEx\". See distrExMASK().")
buildStartupMessage(pkg = "distrEx", msga, msgb, library = library, packageHelp = TRUE, 
#                    MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
VIGNETTE = gettext("Package \"distrDoc\" provides a vignette to this package as well as to several related packages; try vignette(\"distr\")."))
  invisible()
}

distrExMASK <- function(library = NULL) 
{
    infoShow(pkg = "distrEx", filename = "MASKING", library = library)
}


.onUnload <- function(libpath)
{
    library.dynam.unload("distrEx", libpath)
}


# multivariate distribution
setClass("MultivariateDistribution", 
            prototype = prototype(r = function(n){ matrix(rep(c(0,0), n), ncol=2) }, 
                                  d = NULL, p = NULL, q = NULL, param = NULL,
                                  img = new("EuclideanSpace", dimension = 2),
                                  support = matrix(c(0,0), ncol = 2)),
            contains = "Distribution")

# discrete mulitvariate distribution
setClass("DiscreteMVDistribution", representation(support = "matrix"),
            prototype(r = function(n){ matrix(rep(c(0,0), n), ncol=2) }, 
                      d = NULL, p = NULL, q = NULL, param = NULL,
                      img = new("EuclideanSpace", dimension = 2),
                      support = matrix(c(0,0), ncol = 2)),
            contains = "MultivariateDistribution")

# condition
setClass("Condition", representation(name = "character"),
            prototype(name = "a condition"))

# conditioning by an Euclidean space
setClass("EuclCondition", 
            representation(Range = "EuclideanSpace"), 
            prototype(name = gettext("conditioning by an Euclidean space"),
                      Range = new("EuclideanSpace")),
            contains = "Condition")

## conditional univariate distribution
setClass("UnivariateCondDistribution", 
            representation(cond = "Condition"), 
            prototype(r = function(n, cond){ rnorm(n, mean = 0, sd = 1) },
                      d = NULL, p = NULL, q = NULL, img = new("Reals"), 
                      param = NULL, cond = new("Condition")),
            contains = "Distribution")

# absolutely continuous conditional distribution
setClass("AbscontCondDistribution", 
            representation(cond = "Condition"), 
            prototype(r = function(n, cond){ rnorm(n, mean = 0, sd = 1) },
                      d = NULL, p = NULL, q = NULL, img = new("Reals"), 
                      param = NULL, cond = new("Condition")),
            contains = "UnivariateCondDistribution")

# discrete conditional distribution
setClass("DiscreteCondDistribution", 
            representation(support = "function", 
                           cond = "Condition"), 
            prototype(r = function(n, cond){ rep(0, n) },
                      d = NULL, p = NULL, q = NULL, img = new("Reals"), 
                      param = NULL, support = function(cond){0},
                      cond = new("Condition")),
            contains = "UnivariateCondDistribution")

# parameter of Gumbel distribution
setClass("GumbelParameter", representation(loc = "numeric", 
                                           scale = "numeric"), 
            prototype(name = gettext("parameter of a Gumbel distribution"),
                      loc = 0, scale = 1),
            contains = "Parameter",
            validity = function(object){
                if(length(object@scale) != 1)
                    stop("length of 'scale' is not equal to 1")
                if(length(object@loc) != 1)
                    stop("length of 'loc' is not equal to 1")
                if(object@scale <= 0)
                    stop("'scale' has to be positive")
                else return(TRUE)
            })

# Gumbel distribution
setClass("Gumbel", 
            prototype = prototype(r = function(n){ rgumbel(n, loc = 0, scale = 1) },
                                  d = function(x, log){ dgumbel(x, loc = 0, scale = 1, log = FALSE) },
                                  p = function(q, lower.tail = TRUE, log.p = FALSE){ 
                                         p0 <- pgumbel(q, loc = 0, scale = 1, lower.tail = lower.tail)
                                         if(log.p) return(log(p0)) else return(p0) 
                                  },
                                  q = function(p, loc = 0, scale = 1, lower.tail = TRUE, log.p = FALSE){
                                      ## P.R.: changed to vectorized form 
                                      p1 <- if(log.p) exp(p) else p
                                                                                      
                                      in01 <- (p1>1 | p1<0)
                                      i01 <- .isEqual01(p1) 
                                      i0 <- (i01 & p1<1)   
                                      i1 <- (i01 & p1>0)
                                      ii01 <- .isEqual01(p1) | in01
                                                    
                                      p0 <- p
                                      p0[ii01] <- if(log.p) log(0.5) else 0.5
                                                    
                                      q1 <- qgumbel(p0, loc = 0, scale = 1, 
                                                    lower.tail = lower.tail) 
                                      q1[i0] <- if(lower.tail) -Inf else Inf
                                      q1[i1] <- if(!lower.tail) -Inf else Inf
                                      q1[in01] <- NaN
                                      
                                      return(q1)  
                                      },
                                  img = new("Reals"),
                                  param = new("GumbelParameter"),
                                  .logExact = FALSE,
                                  .lowerExact = TRUE),
            contains = "AbscontDistribution")

# Parameter of a linear regression model (with intercept and scale)
setClass("LMParameter", 
            representation(theta = "numeric",
                           intercept = "numeric",
                           scale = "numeric"), 
            prototype(name = gettext("parameter of a linear regression model"),
                      theta = 0, intercept = 0, scale = 1),
            contains = "Parameter",
            validity = function(object){
                if(any(!is.finite(object@theta)))
                    stop("inifinite or missing values in 'theta'")
                if(length(object@intercept) != 1)
                    stop("'intercept' has to be of length 1")
                if(!is.finite(object@intercept))
                    stop("inifinite or missing value in 'intercept'")
                if(length(object@scale) != 1)
                    stop("'scale' has to be of length 1")
                if(!is.finite(object@scale))
                    stop("inifinite or missing value in 'scale'")
                return(TRUE)
            })


###### Pareto distribution by Nataliya Horbenko, ITWM, 18-03-09
## Class: ParetoParameter
setClass("ParetoParameter", 
          representation = representation(shape = "numeric",
                                          Min = "numeric"
                                          ), 
          prototype = prototype(shape = 1, Min = 1, name = 
                      gettext("Parameter of a Pareto distribution")
                      ), 
          contains = "Parameter"
          )

## Class: Pareto distribution
setClass("Pareto",  
          prototype = prototype(
                      r = function(n){ rpareto1(n, shape = 1, min = 1) },
                      d = function(x, log = FALSE){ 
                              dpareto1(x, shape = 1, min = 1, log = log) 
                                          },
                      p = function(q, lower.tail = TRUE, log.p = FALSE ){ 
                              ppareto1(q, shape = 1, min = 1, 
                                     lower.tail = lower.tail, log.p = log.p) 
                                          },
                      q = function(p, lower.tail = TRUE, log.p = FALSE ){ 
                        ## P.R.: changed to vectorized form 
                               p1 <- if(log.p) exp(p) else p
                                                                               
                               in01 <- (p1>1 | p1<0)
                               i01 <- .isEqual01(p1) 
                               i0 <- (i01 & p1<1)   
                               i1 <- (i01 & p1>0)
                               ii01 <- .isEqual01(p1) | in01
                                             
                               p0 <- p
                               p0[ii01] <- if(log.p) log(0.5) else 0.5
                                             
                               q1 <- qpareto1(p0, shape = 1,  min =  1, 
                                           lower.tail = lower.tail, log.p = log.p) 
                               q1[i0] <- if(lower.tail) -Inf else Inf
                               q1[i1] <- if(!lower.tail) -Inf else Inf
                               q1[in01] <- NaN
                               
                               return(q1)  
                            },
                      param = new("ParetoParameter"),
                      img = new("Reals"),
                      .logExact = TRUE,
                      .lowerExact = TRUE),
          contains = "AbscontDistribution"
          )

## Class: GParetoParameter
setClass("GParetoParameter", 
          representation = representation(loc = "numeric", scale = "numeric", shape = "numeric"
                                          ), 
          prototype = prototype(loc = 0, scale = 1, shape = 0, name = 
                      gettext("Parameter of a generalized Pareto distribution")
                      ), 
          contains = "Parameter"
          )
## Class: Generalized Pareto distribution
setClass("GPareto",  
          prototype = prototype(
                      r = function(n){ rgpd(n,loc = 0, scale = 1, shape = 1) },
                      d = function(x, log = FALSE){ 
                              dgpd(x, loc = 0, scale = 1, shape = 1, log = log) 
                                          },
                      p = function(q, lower.tail = TRUE, log.p = FALSE ){ 
                              p0 <- pgpd(q, loc = 0, scale = 1, shape = 1)
                              if(!lower.tail ) p0 <- 1-p0
                              if(log.p) p0 <- log(p0)
                              return(p0)},
                      q = function(p, lower.tail = TRUE, log.p = FALSE ){ 
                        ## P.R.: changed to vectorized form 
                               p1 <- if(log.p) exp(p) else p
                               if(!lower.tail) p1 <- 1-p1
                                                                               
                               in01 <- (p1>1 | p1<0)
                               i01 <- .isEqual01(p1) 
                               i0 <- (i01 & p1<1)   
                               i1 <- (i01 & p1>0)
                               ii01 <- .isEqual01(p1) | in01
                                             
                               p0 <- p
                               p0[ii01] <- if(log.p) log(0.5) else 0.5
                                             
                               q1 <- qgpd(p0,loc=0, scale = 1, shape = 1) 
                               q1[i0] <- if(lower.tail) -Inf else Inf
                               q1[i1] <- if(!lower.tail) -Inf else Inf
                               q1[in01] <- NaN
                               
                               return(q1)  
                            },
                      param = new("GParetoParameter"),
                      img = new("Reals"),
                      .withArith = FALSE,
                      .withSim = FALSE,
                      .logExact = TRUE,
                      .lowerExact = TRUE),
          contains = "AbscontDistribution"
          )
