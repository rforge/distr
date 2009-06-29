\name{MultivarDistrList}
\alias{MultivarDistrList}

\title{Generating function for MultivarDistrList-class}
\description{Generates an object of class \code{"MultivarDistrList"}.}
\usage{MultivarDistrList(...)}
\arguments{
  \item{\dots}{Objects of class \code{"MultivariateDistribution"} which
    shall form the list of multivariate distributions.}
}
%\details{}
\value{Object of class \code{"MultivarDistrList"}}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link[distr]{DistrList-class}}, \code{\link{MultivarDistrList-class}},
  \code{\link{MultivarDistrList}}}
\examples{
(DL <- MultivarDistrList(Norm(), Exp(), Pois()))
plot(DL)
as(Norm(), "MultivarDistrList")

## The function is currently defined as
function(...){ 
    new("MultivarDistrList", list(...)) 
}
}
\keyword{list}
\keyword{distribution}
\concept{univariate distribution}
\concept{S4 distribution class}

