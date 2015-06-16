\name{STd}
\alias{STd}

\title{Generating function for standardized T distribution class}
\description{
  Generates a scaled object of class \code{"Td"};
  the scale (sd) is chosen such that \code{STd(nu=3, sd=1)} has variance 1
  independently from the degrees of freedom \code{nu}. This object
  is of class \code{"AffLinAbscontDistribution"}.
}
\usage{STd(mean = 0, sd = 1, nu = 5)}
\arguments{
  \item{mean}{ real number: location parameter of
    the STd distribution. }
   \item{sd}{ positive real number: scale parameter
    of the STd distribution }
  \item{nu}{ real number larger than 2: degree of freedom parameter of
    the STd distribution.}
 
}
%\details{}
\value{Object of class \code{"STd"}}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\note{This class is based on the code provided by the package \pkg{fGarch}
by  Diethelm Wuertz}
\seealso{\code{\link[fGarch:dist-std]{dstd}}, \code{\link[distr]{AbscontDistribution-class}}}
\examples{
(ST <- STd(mean = 1, sd = 1, nu = 3))
plot(ST)
}

\concept{STd}
\keyword{distribution}
\concept{absolutely continuous distribution}
\concept{STd distribution}
\concept{generating function}
