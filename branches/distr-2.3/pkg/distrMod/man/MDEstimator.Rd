\name{MDEstimator}
\alias{MDEstimator}

\title{ Function to compute minimum distance estimates }
\description{
  The function \code{MDEstimator} provides a general way to compute 
  minimum distance estimates.
}
\usage{
MDEstimator(x, ParamFamily, distance = KolmogorovDist, dist.name, 
            paramDepDist = FALSE, startPar = NULL,  Infos, trafo = NULL,
            penalty = 1e20, asvar.fct, na.rm = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ (empirical) data }
  \item{ParamFamily}{ object of class \code{"ParamFamily"} }
  \item{distance}{ (generic) function: to compute distance beetween (emprical)
  data and objects of class \code{"Distribution"}. }
  \item{dist.name}{ optional name of distance }
  \item{paramDepDist}{logical; will computation of distance be parameter
                      dependent (see also note below)? if \code{TRUE}, distance function
                      must be able to digest a parameter \code{thetaPar}; otherwise
                      this parameter will be eliminated if present in \code{...}-argument.}
  \item{startPar}{ initial information used by \code{optimize} resp. \code{optim};
    i.e; if (total) parameter is of length 1, \code{startPar} is 
    a search interval, else it is an initial parameter value; if \code{NULL}
    slot \code{startPar} of \code{ParamFamily} is used to produce it;
    in the multivariate case, \code{startPar} may also be of class \code{Estimate},
    in which case slot \code{untransformed.estimate} is used.}
  \item{Infos}{ character: optional informations about estimator }
  \item{trafo}{ an object of class \code{MatrixorFunction} -- a transformation
  for the main parameter}
  \item{penalty}{(non-negative) numeric: penalizes non valid parameter-values}
  \item{asvar.fct}{optionally: a function to determine the corresponding
    asymptotic variance; if given, \code{asvar.fct} takes arguments
    \code{L2Fam}((the parametric model as object of class \code{L2ParamFamily})) 
    and \code{param} (the parameter value as object of class 
    \code{ParamFamParameter}); arguments are called by name; \code{asvar.fct}
     may also process further arguments passed through the \code{\dots} argument}              
  \item{na.rm}{logical: if  \code{TRUE}, the estimator is evaluated at \code{complete.cases(x)}.}
  \item{\dots}{ further arguments to \code{criterion} or \code{optimize}
    or \code{optim}, respectively. }
}
\details{
  The argument \code{distance} has to be a (generic) function with arguments 
  the empirical data as well as an object of class \code{"Distribution"} 
  and possibly \code{\dots}; e.g. \code{KolmogorovDist} (default), 
  \code{TotalVarDist} or \code{HellingerDist}. Uses \code{\link{mceCalc}}
  for method dispatch.
}
\value{
  An object of S4-class \code{"MCEstimate"} which inherits from class 
  \code{"Estimate"}.
}
\note{The distance function may be called together with a parameter \code{thetaPar}
 which is the current parameter value under consideration, i.e.; the value
 under which the model distribution is considered. Hence, if desired,
 particular distance functions could make use of this information, by, say
 computing the distance differently for different parameter values.}
\references{
    Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.

    Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{ParamFamily-class}}, \code{\link{ParamFamily}}, 
         \code{\link{MCEstimator}}, \code{\link{MCEstimate-class}},
          \code{\link[MASS]{fitdistr}} }
\examples{
## (empirical) Data
x <- rgamma(50, scale = 0.5, shape = 3)

## parametric family of probability measures
G <- GammaFamily(scale = 1, shape = 2)

## Kolmogorov(-Smirnov) minimum distance estimator
MDEstimator(x = x, ParamFamily = G, distance = KolmogorovDist)

## von Mises minimum distance estimator with default mu
MDEstimator(x = x, ParamFamily = G, distance = CvMDist)

## von Mises minimum distance estimator with default mu
MDEstimator(x = x, ParamFamily = G, distance = CvMDist,
            asvar.fct = distrMod:::.CvMMDCovariance)
#*** variance routine is still in testing phase so not yet
#*** exported to namespace
## von Mises minimum distance estimator with mu = N(0,1)
MDEstimator(x = x, ParamFamily = G, distance = CvMDist, mu = Norm())

## Total variation minimum distance estimator
## gamma distributions are discretized
MDEstimator(x = x, ParamFamily = G, distance = TotalVarDist)
## or smoothing of emprical distribution (takes some time!)
#MDEstimator(x = x, ParamFamily = G, distance = TotalVarDist, asis.smooth.discretize = "smooth")

## Hellinger minimum distance estimator
## gamma distributions are discretized
distroptions(DistrResolution = 1e-10)
MDEstimator(x = x, ParamFamily = G, distance = HellingerDist, startPar = c(1,2))
distroptions(DistrResolution = 1e-6) # default
## or smoothing of emprical distribution (takes some time!)
#MDEstimator(x = x, ParamFamily = G, distance = HellingerDist, asis.smooth.discretize = "smooth")
}
\keyword{univar}
\keyword{robust}