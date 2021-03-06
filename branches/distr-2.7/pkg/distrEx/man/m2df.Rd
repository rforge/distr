\name{m2df}
\alias{m2df}
\alias{m2df-methods}
\alias{m2df,UnivariateDistribution-method}
\alias{m2df,AbscontDistribution-method}
%\alias{m2df,DiscreteDistribution-method}
\alias{m2df,LatticeDistribution-method}
\alias{m2df,AffLinDistribution-method}
\alias{m2df,Binom-method}
\alias{m2df,Pois-method}
\alias{m2df,Norm-method}
\alias{m2df,Exp-method}
\alias{m2df,Chisq-method}

\title{Generic function for the computation of clipped second moments}
\description{
  Generic function for the computation of clipped second moments.
  The moments are clipped at \code{upper}.
}
\usage{
m2df(object, upper, ...)
\S4method{m2df}{AbscontDistribution}(object, upper, 
             lowerTruncQuantile = getdistrExOption("m2dfLowerTruncQuantile"),
             rel.tol = getdistrExOption("m2dfRelativeTolerance"), ...)
}
\arguments{
  \item{object}{ object of class \code{"Distribution"} }
  \item{upper}{ clipping bound }
  \item{rel.tol}{relative tolerance for \code{distrExIntegrate}.} 
  \item{lowerTruncQuantile}{lower quantile for quantile based integration range.}
  \item{\dots}{ additional arguments to \code{E} }
}
\details{The precision of the computations can be controlled via 
  certain global options; cf. \code{\link{distrExOptions}}. }
\value{
  The second moment of \code{object} clipped at \code{upper} is computed.
}
\section{Methods}{
\describe{

  \item{object = "UnivariateDistribution":}{ %
    %clipped first moment
    %for univariate distributions which is computed using 
    %crude Monte-Carlo integration. 
     uses call \code{E(object, upp=upper, fun = function, ...)}.
    }

  \item{object = "AbscontDistribution":}{ clipped second moment
    for absolutely continuous univariate distributions which is 
    computed using \code{integrate}. }

  \item{object = "LatticeDistribution":}{ clipped second moment
    for discrete univariate distributions which is computed 
    using \code{support} and \code{sum}. }

  \item{object = "AffLinDistribution":}{ clipped second moment
    for affine linear distributions  which is computed on basis of 
    slot \code{X0}. }

  \item{object = "Binom":}{ clipped second moment
    for Binomial distributions which is computed using \code{pbinom}. }

  \item{object = "Pois":}{ clipped second moment
    for Poisson distributions which is computed using \code{ppois}. }

  \item{object = "Norm":}{ clipped second moment
    for normal distributions which is computed using \code{dnorm} and \code{pnorm}. }

  \item{object = "Exp":}{ clipped second moment
    for exponential distributions which is computed using \code{pexp}. }

  \item{object = "Chisq":}{ clipped second moment
    for \eqn{\chi^2}{Chi^2} distributions which is computed using \code{pchisq}. }
}}
%\references{ ~put references to the literature/web site here ~ }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{m2df-methods}}, \code{\link{E-methods}}}
\examples{
# standard normal distribution
N1 <- Norm()
m2df(N1, 0)

# Poisson distribution
P1 <- Pois(lambda=2)
m2df(P1, 3)
m2df(P1, 3, fun = function(x)sin(x))

# absolutely continuous distribution
D1 <- Norm() + Exp() # convolution
m2df(D1, 2)
m2df(D1, Inf)
E(D1, function(x){x^2})
}
\concept{moment}
\concept{functional}
\concept{truncated moment}
\keyword{methods}
\keyword{distribution}
