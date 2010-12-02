\name{EllipticalDistribution}
\alias{EllipticalDistribution}

\title{Generating function for EllipticalDistribution-class}
\description{
  Generates an object of class \code{"EllipticalDistribution"}.
}
\usage{EllipticalDistribution(radDistr = sqrt(Chisq(df = length(loc))),
                            loc = c(0,0), scale = diag(length(loc)), p = NULL, q = NULL)}
\arguments{
  \item{radDistr}{an object of class \code{UnivariateDistribution} with positive 
                  support, i.e. \code{p(radDistr)(0)==0}; the radial distribution. }
  \item{loc}{ real number: location / center of the elliptical distribution.} 
  \item{scale}{ a square matrix (with \code{nrow(scale)==ncol(scale)==length(loc)})
                of full rank: the / a scale matrix of the elliptical distribution
                --- unique only upto \code{scale\%*\%t(scale)}, i.e. if \code{A1} and
                \code{A2} are two square matrices of full rank such that 
                \code{A1\%*\%t(A1)==A2\%*\%t(A2)},
                then we obtain the same elliptical distribution
                for \code{scale = A1} and for \code{scale = A2}.} 
 \item{p}{ optional: \code{p}-slot of the corresponding distribution;} 
 \item{q}{ optional: \code{q}-slot of the corresponding distribution;} 
}
%\details{}
\value{Object of class \code{"EllipticalDistribution"}}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\seealso{\code{\link{EllipticalDistribution-class}}}
\examples{
E0 <- EllipticalDistribution()
plot(E0)
E1 <- diag(1,2)\%*\%E0+c(1,2)
plot(E1)
E(E1)
var(E1)
}

\concept{EllipticalDistribution}
\keyword{distribution}
\concept{absolutely continuous distribution}
\concept{elliptical distribution}
\concept{generating function}
