\name{MultivarMixingDistribution}
\alias{MultivarMixingDistribution}

\title{Generating function for Class "MultivarMixingDistribution"}
\description{Generates an object of class \code{"MultivarMixingDistribution"}.}
\usage{MultivarMixingDistribution(..., Dlist, mixCoeff%,
                    %            withSimplify = getdistrOption("simplifyD")
                    )}
\arguments{
  \item{\dots}{Objects of class \code{"MultivariateDistribution"} (or subclasses)}
  \item{Dlist}{an optional list or object of class \code{"MultivarDistrList"};
  if not missing it is appended to argument \code{\dots}; this way 
  \code{MultivarMixingDistribution} may also be called with a list (or 
  \code{"MultivarDistrList"}-object) as argument as suggested in an e-mail
  by Krunoslav Sever (thank you!)}
  \item{mixCoeff}{Objects of class \code{"numeric"} : a vector of 
            probabilities for the mixing components (must be of same length as
            arguments in \dots).}
%  \item{withSimplify}{\code{"logical"}: shall the return value be piped through a
%   call to \code{simplifyD}?}
}
\details{If \code{mixCoeff} is missing, all elements in \code{\dots}
  are equally weighted.}
\value{Object of class \code{"MultivarMixingDistribution"}, or if 
argument \code{withSimplify} is \code{TRUE} and the resulting 
object would have one mixing component with probability (almost) 1,
\code{MultivarMixingDistribution} will return this component.}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{MultivarMixingDistribution-class}}%,
%  \code{\link[distr]{simplifyD}}
  }
\examples{
mylist <- MultivarMixingDistribution(Binom(3,.3), Dirac(2), Norm(), 
          mixCoeff=c(1/4,1/5,11/20))
}
\keyword{distribution}
\keyword{list}
\concept{multivariate distribution}
\concept{S4 distribution class}
\concept{generating function}

