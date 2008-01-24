\name{make01}
\alias{make01}

\title{Centering and Standardization of Univariate Distributions}
\description{
  The function \code{make01} produces a new centered and 
  standardized univariate distribution.
}
\usage{
make01(x)
}
\arguments{
  \item{x}{ an object of class \code{"UnivariateDistribution"} }
  }
\details{
  Thanks to the functionals provided in this package, the code is 
  a one-liner: \code{(x-E(x))/sd(x)}.
}
\value{Object of class \code{"UnivariateDistribution"} with expectation 0 and variance 1.}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-bayreuth.de}}
%\note{}
\seealso{\code{E}, \code{Var}}
\examples{
X <- sin(exp(2*log(abs( Norm())))) ## something weird
X01 <- make01(X)
print(X01)
plot(X01)
sd(X01); E(X01)
}
\concept{functional}
\concept{centering}
\concept{standardization}
\keyword{distribution}

