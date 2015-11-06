\name{EmpiricalMVDistribution}
\alias{EmpiricalMVDistribution}

\title{Generating function for mulitvariate discrete distribution}
\description{
  Generates an object of class \code{"DiscreteMVDistribution"}.
}
\usage{
EmpiricalMVDistribution(data, Symmetry = NoSymmetry())
}
\arguments{
  \item{data}{ numeric matrix with data where the rows are 
    interpreted as observations. }
  \item{Symmetry}{you may help \R in calculations if you tell it whether
    the distribution is non-symmetric (default) or symmetric with respect
    to a center.}
}
\details{
  The function is a simple utility function providing a wrapper to the 
  generating function \code{\link{DiscreteDistribution}}.
  
  Typical usages are
  \preformatted{
    EmpiricalMVDistribution(data)
  }
   
  Identical rows are collapsed to unique support values. 
  If \code{prob} is missing, all elements in \code{supp}
  are equally weighted. 
}
\value{Object of class \code{"DiscreteMVDistribution"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{DiscreteMVDistribution}}}
\examples{
## generate some data
X <- matrix(rnorm(50), ncol = 5)

## empirical distribution of X
D1 <- EmpiricalMVDistribution(data = X)
support(D1)
r(D1)(10)
}
\concept{multivariate distribution}
\concept{empirical distribution}
\keyword{distribution}
\concept{S4 distribution class}
\concept{generating function}
