\name{MVNormDistribution-class}
\docType{class}
\alias{MVNormDistribution-class}
\alias{sigma,MVNormDistribution-method}
\alias{mean,MVNormDistribution-method}


\title{MVNorm distribution class}
\description{
Class \code{MVNormDistribution} implements a general multivariate distribution
using code from package \pkg{mvtnorm}. For details to this implementation confer
to the references given in this package. 
}
\section{Objects from the Class}{
  Objects could in principle be created by calls to \code{new}, but more 
  frequently you would create them  via the generating function 
  \code{\link{MVNormDistribution}}.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}. }
    \item{\code{param}:}{Object of class \code{"MVtParameter"}. }
    \item{\code{r}:}{function with argument \code{n}; random number generator}
    \item{\code{d}:}{ the density of this distribution, \code{\link[mvtnorm]{pmvnorm}}}
    \item{\code{p}:}{the (vectorized) function \code{\link[mvtnorm]{pmvnorm}}.}
    \item{\code{q}:}{the (vectorized) function \code{\link[mvtnorm]{qmvnorm}}.}
    \item{\code{radDistr}:}{the distribution \code{sqrt(Chisq(df=dim0))}}
    \item{\code{.withArith}:}{FALSE}
    \item{\code{.withSim}:}{FALSE}
    \item{\code{.logExact}:}{TRUE}
    \item{\code{.lowerExact}:}{TRUE}
    \item{\code{Symmetry}:}{object of class \code{"EllipticalSymmetry"} about 
    center \code{loc}; used internally to avoid unnecessary calculations.}
}}
\section{Extends}{
Class \code{"EllipticalDistribution"}, directly. \cr
Class \code{"SphericalDistribution"}, by class \code{"EllipticalDistribution"}. \cr
Class \code{"MultivariateDistribution"}, by class \code{"SphericalDistribution"}.
Class \code{"Distribution"}, by class \code{"MultivariateDistribution"}.
}

\section{Methods}{
  \describe{
    \item{sigma}{\code{signature(object = "MVNormDistribution")}: wrapped access method for
      slot \code{sigma} of slot \code{param}. }

    \item{mean}{\code{signature(object = "MVNormDistribution")}: wrapped access method for
      slot \code{location}  of slot \code{param}. }

  }
}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
new("MVNormDistribution") ## better use generating function MVNormDistribution()
}
\seealso{Package \pkg{mvtnorm}}
\concept{Multivariate normal Distribution}
\keyword{distribution}
\concept{extreme value distribution}
\concept{absolutely continuous distribution}
\concept{S4 distribution class}
