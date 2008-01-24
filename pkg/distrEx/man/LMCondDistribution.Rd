\name{LMCondDistribution}
\alias{LMCondDistribution}

\title{Generating function for the conditional distribution 
  of a linear regression model.}
\description{
  Generates an object of class \code{"AbscontCondDistribution"} which 
  is the conditional distribution of a linear regression model (given
  the regressor).
}
\usage{
LMCondDistribution(Error = Norm(), theta = 0, intercept = 0, scale = 1)
}
\arguments{
  \item{Error}{ Object of class \code{"AbscontDistribution"}: 
    error distribution. }
  \item{theta}{ numeric vector: regression parameter. }
  \item{intercept}{ real number: intercept parameter. }
  \item{scale}{ positive real number: scale parameter. }
}
\value{Object of class \code{"AbscontCondDistribution"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{AbscontCondDistribution-class}}, \code{\link{E-methods}}}
\examples{
# normal error distribution
(D1 <- LMCondDistribution(theta = 1)) # corresponds to Norm(cond, 1)
plot(D1)
r(D1)
d(D1)
p(D1)
q(D1)
param(D1)
cond(D1)

d(D1)(0, cond = 1)
d(Norm(mean=1))(0)

E(D1, cond = 1)
E(D1, function(x){x^2}, cond = 2)
E(Norm(mean=2), function(x){x^2})
}
\concept{conditional distribution}
\keyword{distribution}
\keyword{models}
\concept{S4 distribution class}
\concept{generating function}
