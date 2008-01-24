\name{MultivariateDistribution-class}
\docType{class}
\alias{MultivariateDistribution-class}
\alias{show,MultivariateDistribution-method}
\alias{plot,MultivariateDistribution-method}

\title{Multivariate Distributions}
\description{The class of multivariate distributions. One has at least to 
  specify the image space of the distribution and a function generating
  (pseudo-)random numbers. The slot \code{q} is usually filled with 
  \code{NULL} for dimensions \code{> 1}.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("MultivariateDistribution", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{ Object of class \code{"rSpace"}. 
      Image space of the distribution. Usually an object of 
      class \code{"EuclideanSpace"}.}
    \item{\code{param}:}{Object of class \code{"OptionalParameter"}.
      Optional parameter of the multivariate distribution.}
    \item{\code{r}:}{Object of class \code{"function"}: 
      generates (pseudo-)random numbers}
    \item{\code{d}:}{Object of class \code{"OptionalFunction"}: 
      optional density function}
    \item{\code{p}:}{Object of class \code{"OptionalFunction"}: 
      optional cumulative distribution function }
    \item{\code{q}:}{Object of class \code{"OptionalFunction"}: 
      optional quantile function }
  }
}
\section{Extends}{
Class \code{"Distribution"}, directly.
}
\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "MultivariateDistribution")}}

  \item{plot}{\code{signature(object = "MultivariateDistribution")}:
    not yet implemented.}
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distr]{Distribution-class}}}
\examples{
# Dirac-measure in (0,0)
new("MultivariateDistribution")
}
\concept{multivariate distribution}
\keyword{distribution}
\concept{S4 distribution class}
