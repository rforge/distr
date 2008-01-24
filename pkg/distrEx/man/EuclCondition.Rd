\name{EuclCondition}
\alias{EuclCondition}

\title{Generating function for EuclCondition-class}
\description{
  Generates an object of class \code{"EuclCondition"}.
}
\usage{EuclCondition(dimension)}
\arguments{
  \item{dimension}{positive integer: dimension of the Euclidean space}
}
\value{Object of class \code{"EuclCondition"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{EuclCondition-class}}}
\examples{
EuclCondition(dimension = 3)

## The function is currently defined as
function(dimension){
    new("EuclCondition", Range = EuclideanSpace(dimension = dimension))
}
}
\concept{generating function}
\concept{conditioning}
\concept{condition}
\concept{S4 condition class}
\concept{conditional distribution}
\keyword{distribution}
