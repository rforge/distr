\name{SNorm}
\alias{SNorm}

\title{Generating function for skewed normal class}
\description{
  Generates an object of class \code{"SNorm"}.
}
\usage{SNorm(mean = 0, sd = 1, xi = 1.5)}
\arguments{
  \item{mean}{ real number: location parameter of
    the SNorm distribution. }
   \item{sd}{ positive real number: scale parameter
    of the SNorm distribution }
   \item{xi}{ positive real number: shape parameter of
    the SSTd distribution. }
}
%\details{}
\value{Object of class \code{"SNorm"}}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@itwm.fraunhofer.de}}
\note{This class is based on the code provided by the package \pkg{fGarch}
by  Diethelm Wuertz}
\seealso{\code{\link[fGarch:snorm]{dsnorm}}, \code{\link[distr]{AbscontDistribution-class}}}
\examples{
(SN <- SNorm(mean = 1, sd = 1, xi = 0.5))
plot(SN)
}

\concept{SNorm}
\keyword{distribution}
\concept{absolutely continuous distribution}
\concept{SNorm distribution}
\concept{generating function}
