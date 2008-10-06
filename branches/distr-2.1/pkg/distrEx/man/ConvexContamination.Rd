\name{ConvexContamination}
\alias{ConvexContamination}
\alias{ConvexContamination-methods}
\alias{ConvexContamination,UnivariateDistribution,UnivariateDistribution,numeric-method}
\alias{ConvexContamination,AbscontDistribution,AbscontDistribution,numeric-method}
\alias{ConvexContamination,AbscontDistribution,UnivariateDistribution,numeric-method}
\alias{ConvexContamination,DiscreteDistribution,DiscreteDistribution,numeric-method}
\alias{ConvexContamination,AcDcLcDistribution,AcDcLcDistribution,numeric-method}

\title{Generic Function for Generating Convex Contaminations}
\description{Generic function for generating convex contaminations. This is also 
  known as \emph{gross error model}. Given two distributions \eqn{P} 
  (ideal distribution), \eqn{R} (contaminating distribution) and the
  size \eqn{\varepsilon\in [0,1]} the convex contaminated distribution
  \deqn{Q = (1-\varepsilon)P + \varepsilon R}{Q = (1-epsilon)P + epsilon R}
  is generated.}
\usage{ConvexContamination(e1, e2, size)
}
\arguments{
  \item{e1}{ object of class \code{"Distribution"}: ideal distribution}
  \item{e2}{ object of class \code{"Distribution"}: contaminating distribution}
  \item{size}{ size of contamination (amount of gross errors)}
}
%\details{}
\value{Object of class \code{"Distribution"}.}
\section{Methods}{
\describe{
  \item{e1 = "UnivariateDistribution", e2 = "UnivariateDistribution", size = "numeric":}{ 
    convex combination of two univariate distributions }

  \item{e1 = "AbscontDistribution", e2 = "AbscontDistribution", size = "numeric":}{ 
    convex combination of two absolutely continuous univariate distributions }

  \item{e1 = "DiscreteDistribution", e2 = "DiscreteDistribution", size = "numeric":}{
    convex combination of two discrete univariate distributions }

  \item{e1 = "AcDcLcDistribution", e2 = "AcDcLcDistribution", size = "numeric":}{
    convex combination of two univariate distributions which may be coerced to
    \code{"UnivarLebDecDistribution"}.}
}}
\references{Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{ContaminationSize}}, \code{\link[distr]{Distribution-class}}}
\examples{
# Convex combination of two normal distributions
C1 <- ConvexContamination(e1 = Norm(), e2 = Norm(mean = 5), size = 0.1)
plot(C1)
}
\concept{convex contamination}
\concept{gross error}
\keyword{distribution}
\concept{univariate distribution}
\concept{absolutely continuous distribution}
\concept{discrete distribution}
\keyword{methods}
\concept{S4 distribution class}
