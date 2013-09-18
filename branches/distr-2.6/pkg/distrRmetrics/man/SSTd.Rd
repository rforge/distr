\name{SSTd}
\alias{SSTd}

\title{Generating function for SSTd-class}
\description{
  Generates an object of class \code{"SSTd"}.
}
\usage{SSTd(mean = 0, sd = 1, nu = 5, xi = 1.5)}
\arguments{
  \item{mean}{ real number: location parameter of
    the SSTd distribution. }
   \item{sd}{ positive real number: scale parameter
    of the SSTd distribution }
   \item{xi}{ positive real number: shape parameter of
    the SSTd distribution. }
  \item{nu}{ real number larger than 2: degree of freedom parameter of
    the SSTd distribution.}
 
}
%\details{}
\value{Object of class \code{"SSTd"}}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@itwm.fraunhofer.de}}
\note{This class is based on the code provided by the package \pkg{fGarch}
by  Diethelm Wuertz}
\seealso{\code{\link[fGarch:dist-sstd]{dsstd}}, \code{\link[distr]{AbscontDistribution-class}}}
\examples{
(ST <- SSTd(mean = 1, sd = 1, xi = 0.5))
plot(ST)
}

\concept{SSTd}
\keyword{distribution}
\concept{absolutely continuous distribution}
\concept{SSTd distribution}
\concept{generating function}
