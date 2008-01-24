\name{LMParameter}
\alias{LMParameter}

\title{Generating function for LMParameter-class}
\description{
  Generates an object of class \code{"LMParameter"}.
}
\usage{
LMParameter(theta = 0, intercept = 0, scale = 1)
}
\arguments{
  \item{theta}{ numeric vector: regression parameter (default \code{=0}). }
  \item{intercept}{ real number: intercept parameter (default \code{=0}). }
  \item{scale}{ positive real number: scale parameter (default \code{=1}). }
}
\value{Object of class \code{"LMParameter"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{LMParameter-class}}}
\examples{
LMParameter(theta = c(1,1), intercept = 2, scale = 0.5)

## The function is currently defined as
function(theta = 0, intercept = 0, scale = 1){
    new("LMParameter", theta = theta, intercept = intercept, scale = 1)
}
}
\concept{parameter}
\concept{S4 parameter class}
\keyword{models}
\concept{generating function}
