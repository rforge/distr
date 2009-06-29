\name{MultivarDistrList-class}
\docType{class}
\alias{MultivarDistrList-class}
\alias{coerce,MultivariateDistribution,MultivarDistrList-method}
\alias{dim,MultivarDistrList-method}

\title{List of multivariate distributions}
\description{Create a list of multivariate distributions}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("MultivarDistrList", ...)}.
  More frequently they are created via the generating function 
  \code{\link{MultivarDistrList}}. 
}
\section{Slots}{
  \describe{
      \item{\code{.Data}:}{Object of class \code{"list"}. 
      A list of multivariate distributions of the same dimension.}
  }
}
\section{Extends}{
Class \code{"DistrList"}, directly.\cr
Class \code{"list"}, by class \code{"DistrList"}.\cr
Class \code{"vector"}, by class \code{"DistrList"}.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "MultivariateDistribution", to = "MultivarDistrList")}:
      create a \code{MultivarDistrList} object from a univariate distribution}
    \item{dim}{returns the dimension of the range space.}
  }
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{MultivarDistrList}}, \code{\link[distr]{DistrList-class}},
    \code{\link[distrEx]{MultivariateDistribution-class}}}
\examples{
(DL <- new("MultivarDistrList", list(Norm(), Exp())))
plot(DL)
as(Norm(), "MultivarDistrList")
}
\keyword{distribution}
\keyword{list}
\concept{multivariate distribution}
\concept{S4 distribution class}

