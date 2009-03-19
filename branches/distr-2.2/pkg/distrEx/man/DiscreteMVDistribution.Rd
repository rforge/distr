\name{DiscreteMVDistribution}
\alias{DiscreteMVDistribution}

\title{Generating function for DiscreteMVDistribution-class}
\description{
  Generates an object of class \code{"DiscreteMVDistribution"}.
}
\synopsis{DiscreteMVDistribution(supp, prob)}
\usage{
  DiscreteMVDistribution(supp, prob)
  DiscreteMVDistribution(supp)
}
\arguments{
  \item{supp}{ numeric matrix whose rows form the support 
    of the discrete multivariate distribution. }
  \item{prob}{ vector of probability weights for the 
    elements of \code{supp}.}
}
\details{
  Identical rows are collapsed to unique support values. 
  If \code{prob} is missing, all elements in \code{supp}
  are equally weighted. 
}
\value{Object of class \code{"DiscreteMVDistribution"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{DiscreteMVDistribution-class}}
\examples{
# Dirac-measure at (0,0,0)
D1 <- DiscreteMVDistribution(supp = c(0,0,0))
support(D1)

# simple discrete distribution
D2 <- DiscreteMVDistribution(supp = matrix(c(0,1,0,2,2,1,1,0), ncol=2), 
                prob = c(0.3, 0.2, 0.2, 0.3))
support(D2)
r(D2)(10)
}
\concept{multivariate distribution}
\concept{discrete distribution}
\keyword{distribution}
\concept{S4 distribution class}
\concept{generating function}
