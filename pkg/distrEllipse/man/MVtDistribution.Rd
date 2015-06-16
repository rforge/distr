\name{MVtDistribution}
\alias{MVtDistribution}
\alias{MVt}

\title{Generating function for MvtDistribution-class}
\description{
  Generates an object of class \code{"MvtDistribution"}.
}
\usage{MVt(loc = c(0,0), scale = diag(length(loc)), df = 1, ncp = 0)}

\arguments{
  \item{loc}{ real number: location / center of the elliptical distribution.} 
  \item{scale}{ a square matrix (with \code{nrow(scale)==ncol(scale)==length(loc)})
                of full rank: the / a scale matrix of the elliptical distribution
                --- unique only upto \code{scale\%*\%t(scale)
                }, i.e. if \code{A1} and 
                \code{A2} are two square matrices of full rank such that 
                \code{A1\%*\%t(A1)==A2\%*\%t(A2)
                }, then we obtain the same elliptical distribution
                for \code{scale = A1} and for \code{scale = A2}.} 
 \item{df}{integer; degrees of freedom} 
 \item{ncp}{positive real number; non-centrality parameter} 
}
%\details{}
\value{Object of class \code{"MvtDistribution"}}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\seealso{\code{\link{MVtDistribution-class}}}
\examples{
E0 <- MVt()
plot(E0)
E1 <- diag(1,2)\%*\%E0+c(1,2)
plot(E1)
E(E1)
var(E1)
}

\concept{MvtDistribution}
\keyword{distribution}
\concept{absolutely continuous distribution}
\concept{elliptical distribution}
\concept{generating function}
