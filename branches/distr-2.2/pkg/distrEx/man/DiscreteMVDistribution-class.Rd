\name{DiscreteMVDistribution-class}
\docType{class}
\alias{DiscreteMVDistribution-class}
\alias{support,DiscreteMVDistribution-method}

\title{Discrete Multivariate Distributions}
\description{The class of discrete multivariate distributions.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("DiscreteMVDistribution", ...)}.
  More frequently they are created via the generating function 
  \code{DiscreteMVDistribution}.
}
\section{Slots}{
  \describe{
    \item{\code{img}}{ Object of class \code{"rSpace"}. 
      Image space of the distribution. Usually an object of 
      class \code{"EuclideanSpace"}.}
    \item{\code{param}}{Object of class \code{"OptionalParameter"}.
      Optional parameter of the multivariate distribution.}
    \item{\code{r}}{Object of class \code{"function"}: 
      generates (pseudo-)random numbers}
    \item{\code{d}}{Object of class \code{"OptionalFunction"}: 
      optional density function}
    \item{\code{p}}{Object of class \code{"OptionalFunction"}: 
      optional cumulative distribution function }
    \item{\code{q}}{Object of class \code{"OptionalFunction"}: 
      optional quantile function }
    \item{\code{support}}{ numeric matrix whose rows form the
      support of the distribution}
    \item{\code{.withArith}}{logical: used internally to issue warnings as to interpretation of arithmetics}
    \item{\code{.withSim}}{logical: used internally to issue warnings as to accuracy}
    \item{\code{.logExact}}{logical: used internally to flag the case where there are explicit formulae for the
                              log version of density, cdf, and quantile function}
    \item{\code{.lowerExact}}{logical: used internally to flag the case where there are explicit formulae for the
                              lower tail version of cdf and quantile function}
  }
}
\section{Extends}{
Class \code{"MultivariateDistribution"}, directly.\cr
Class \code{"Distribution"}, by class \code{"MultivariateDistribution"}.
}
\section{Methods}{
\describe{
  \item{support}{\code{signature(object = "DiscreteMVDistribution")}:
    accessor function for slot \code{support}.}
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distr]{Distribution-class}}, \code{\link{MultivariateDistribution-class}},
  \code{\link{DiscreteMVDistribution}}, \code{\link{E-methods}}}
\examples{
(D1 <- new("MultivariateDistribution")) # Dirac measure in (0,0)
r(D1)(5)

(D2 <- DiscreteMVDistribution(supp = matrix(c(1:5, rep(3, 5)), ncol=2, byrow=TRUE)))
support(D2)
r(D2)(10)
d(D2)(support(D2))
p(D2)(lower = c(1,1), upper = c(3,3))
q(D2)
param(D2)
img(D2)

e1 <- E(D2) # expectation
}
\concept{multivariate distribution}
\concept{discrete distribution}
\keyword{distribution}
\concept{S4 distribution class}
