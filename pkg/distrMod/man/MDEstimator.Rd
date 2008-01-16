\name{MDEstimator}
\alias{MDEstimator}

\title{ Function to compute minimum distance estimators }
\description{
  The function \code{MDEstimator} provides a general way to compute 
  minimum distance estimators.
}
\usage{
MDEstimator(x, ParamFamily, distance = KolmogorovDist, interval, par, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ (empirical) data }
  \item{ParamFamily}{ object of class \code{"ParamFamily"} }
  \item{distance}{ (generic) function: to compute distance beetween (emprical)
  data and objects of class \code{"Distribution"}. }
  \item{interval}{ interval used by \code{optimize} }
  \item{par}{ initial parameter values used by \code{optim}; if \code{missing}
    the parameter of \code{ParamFamily} are used. }
  \item{\dots}{ further arguments to \code{criterion} or \code{optimize} 
    or \code{optim}, respectively. }
}
\details{
  The argument \code{distance} has to be a (generic) function with arguments 
  the empirical data as well as an object of class \code{"Distribution"} 
  and possibly \code{\dots}; e.g. \code{KolmogorovDist} (default), 
  \code{TotalVarDist} or \code{HellingerDist}.
}
\value{
  An object of S3-class \code{"MDEstimator"} which inherits from 
  class \code{"MCEstimator"}, a list with two components
  \item{estimate}{ maximum likelihood parameter estimate }
  \item{distance resp. name of the distance }{ minimum value of the distance }
}
\references{
    Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.

    Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{}
\seealso{\code{\link{ParamFamily-class}}, \code{\link{ParamFamily}}, code{\link{MCEstimator}},
\code{\link[MASS]{fitdistr}} }
\examples{
## (empirical) Data
x <- rgamma(50, scale = 0.5, shape = 3)

## parametric family of probability measures
theta <- c(1, 2)
names(theta) <- c("scale", "shape")
G <- ParamFamily(name = "Gamma family",
        param = ParamFamParameter(name = "scale and shape", main = theta),
        distribution = Gammad(scale = 1, shape = 2),
        modifyParam = function(theta){ 
                          Gammad(scale = theta[1], shape = theta[2]) 
                      },
        props = paste(c("The Gamma family is scale invariant via the",
                  "parametrization '(nu,shape)=(log(scale),shape)'"),
                  collapse = " "))

## Kolmogorov(-Smirnov) minimum distance estimator
MDEstimator(x = x, ParamFamily = G, distance = KolmogorovDist)

## Total variation minimum distance estimator
## gamma distributions are discretized
MDEstimator(x = x, ParamFamily = G, distance = TotalVarDist)
## or smoothing of emprical distribution (takes some time!)
#MDEstimator(x = x, ParamFamily = G, distance = TotalVarDist, asis.smooth.discretize = "smooth")

## Hellinger minimum distance estimator
## gamma distributions are discretized
distroptions(DistrResolution = 1e-8)
MDEstimator(x = x, ParamFamily = G, distance = HellingerDist)
distroptions(DistrResolution = 1e-6) # default
## or smoothing of emprical distribution (takes some time!)
#MDEstimator(x = x, ParamFamily = G, distance = HellingerDist, asis.smooth.discretize = "smooth")
}
\keyword{univar}
