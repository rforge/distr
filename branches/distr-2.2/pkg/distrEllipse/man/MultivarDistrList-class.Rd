\name{MultivarDistrList-class}
\docType{class}
\alias{MultivarDistrList-class}
\alias{MVDistrList-class}
\alias{coerce,MultivariateDistribution,MultivarDistrList-method}
\alias{dim,MultivarDistrList-method}
\alias{dimension,MultivarDistrList-method}

\title{List of multivariate distributions}
\description{Create a list of multivariate distributions}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("MVDistrList", ...)}.
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
    \item{dimension}{dim of the range space.}
    \item{dim}{synonym to dimension.}
  }
}
\section{Details}{
In fact, class \code{"MultivarDistrList"} is an inbetween class between
class \code{"DistrList"} and class \code{"UnivarDistrList"}, which is
a case for \code{\link{setIs}}, but we would have to modify the metadata
information in package \pkg{distr} to realize this. So we introduce
a new (sister) class \code{"MVDistrList"} which implements strictly
lists of multivariate distributions, and which together with
\code{"UnivarDistrList"} is a subclass of the common class union
class \code{"MultivarDistrList"}.
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{MultivarDistrList}}, \code{\link[distr]{DistrList-class}},
    \code{\link[distrEx]{MultivariateDistribution-class}}}
\examples{
(DL1 <- MultivarDistrList(Norm(), Exp(), Pois()))
(DL2 <- MultivarDistrList(MVNorm(),
                          EllipticalDistribution(radDistr=Exp(), loc=c(1,2),
                          scale=diag(c(3,1))),MVt()))

}
\keyword{distribution}
\keyword{list}
\concept{multivariate distribution}
\concept{S4 distribution class}

