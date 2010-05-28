\name{NbinomFamily}
\alias{NbinomFamily}

\title{Generating function for Nbinomial families}
\description{
  Generates an object of class \code{"L2ParamFamily"} which
  represents a Nbinomial family where the probability of
  success is the parameter of interest.
}
\usage{
NbinomFamily(size = 1, prob = 0.5, trafo)
}
\arguments{
  \item{size}{ number of trials }
  \item{prob}{ probability of success }
  \item{trafo}{ function in \code{param} or matrix: transformation of the parameter }
}
\details{
  The slots of the corresponding L2 differentiable 
  parameteric family are filled.
}
\value{Object of class \code{"L2ParamFamily"}}
\references{
  Kohl, M. (2005) \emph{Numerical Contributions to the 
  Asymptotic Theory of Robustness}. Bayreuth: Dissertation.

  Kohl, M. and Ruckdeschel, P. (2010). 
  R Package distrMod: S4 Classes and Methods for
  Probability Models. To appear in Journal of Statistical Software.
}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{L2ParamFamily-class}}, \code{\link[distr]{Nbinom-class}}}
\examples{
(N1 <- NbinomFamily(size = 25, prob = 0.25))
plot(N1)
FisherInfo(N1)
checkL2deriv(N1)
}
\concept{Negative Binomial model}
\keyword{models}
