\name{SphericalDistribution}
\alias{SphericalDistribution}

\title{Generating function for SphericalDistribution-class}
\description{
  Generates an object of class \code{"SphericalDistribution"}.
}
\usage{SphericalDistribution(radDistr = sqrt(Chisq(df=dim)), dim = 2,
                             p = NULL, q = NULL)}



\arguments{
  \item{radDistr}{an object of class \code{UnivariateDistribution} with positive
                  support, i.e. \code{p(radDistr)(0)==0}; the radial distribution. }
  \item{dim}{ positive integer: dimension of the distribution.}
 \item{p}{ optional: \code{p}-slot of the corresponding distribution;}
 \item{q}{ optional: \code{q}-slot of the corresponding distribution;}
}
%\details{}
\value{Object of class \code{"SphericalDistribution"}}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\seealso{\code{\link{SphericalDistribution-class}}}
\examples{
E0 <- SphericalDistribution()
plot(E0)
E1 <- diag(1,2)\%*\%E0+c(1,2)
plot(E1)
E(E1)
var(E1)
}

\concept{SphericalDistribution}
\keyword{distribution}
\concept{absolutely continuous distribution}
\concept{elliptical distribution}
\concept{generating function}
