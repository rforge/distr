\name{MultivarDistrList}
\alias{MultivarDistrList}

\title{Generating function for MultivarDistrList-class}
\description{Generates an object of class \code{"MultivarDistrList"}.}
\usage{MultivarDistrList(..., Dlist)}
\arguments{
  \item{\dots}{Objects of class \code{"MultivariateDistribution"} (or subclasses)}
  \item{Dlist}{an optional list or object of class \code{"MultivarDistrList"};
  if not missing it is appended to argument \code{\dots}; this way
  \code{MultivarMixingDistribution} may also be called with a list (or
  \code{"MultivarDistrList"}-object) as argument as suggested in an e-mail
  by Krunoslav Sever (thank you!)}
}
%\details{}
\value{Object of class \code{"MVDistrList"} or of
class \code{"UnivarDistrList"}, hence of class union \code{"MultivarDistrList"}}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
%\note{}
\seealso{\code{\link[distr]{DistrList-class}}, \code{\link{MultivarDistrList-class}},
  \code{\link{MultivarDistrList}}}
\examples{
(DL1 <- MultivarDistrList(Norm(), Exp(), Pois()))
(DL2 <- MultivarDistrList(MVNorm(),
                          EllipticalDistribution(radDistr=Exp(), loc=c(1,2),
                          scale=diag(c(3,1))),MVt()))
}

\keyword{list}
\keyword{distribution}
\concept{univariate distribution}
\concept{S4 distribution class}

