\name{m1df}
\alias{m1df}
\alias{m1df-methods}
\alias{m1df,UnivariateDistribution-method}
\alias{m1df,AbscontDistribution-method}
\alias{m1df,DiscreteDistribution-method}
\alias{m1df,Binom-method}
\alias{m1df,Pois-method}
\alias{m1df,Norm-method}
\alias{m1df,Exp-method}
\alias{m1df,Chisq-method}

\title{Generic Function for the Computation of Clipped First Moments}
\description{
  Generic function for the computation of clipped first moments.
  The moments are clipped at \code{upper}.
}
\usage{
m1df(object, upper)
}
\arguments{
  \item{object}{ object of class \code{"Distribution"} }
  \item{upper}{ clipping bound }
}
\details{The precision of the computations can be controlled via 
  certain global options; cf. \code{\link{distrExOptions}}. }
\value{
  The first moment of \code{object} clipped at \code{upper} is computed.
}
\section{Methods}{
\describe{
  \item{object = "UnivariateDistribution":}{ clipped first moment
    for univariate distributions which is computed using crude 
    Monte-Carlo integration. }

  \item{object = "AbscontDistribution":}{ clipped first moment
    for absolutely continuous univariate distributions which 
    is computed using \code{distrExIntegrate}. }

  \item{object = "DiscreteDistribution":}{ clipped first moment
    for discrete univariate distributions which is computed 
    using \code{support} and \code{sum}. }

  \item{object = "Binom":}{ clipped first moment
    for Binomial distributions which is computed using \code{pbinom}. }

  \item{object = "Pois":}{ clipped first moment
    for Poisson distributions which is computed using \code{ppois}. }

  \item{object = "Norm":}{ clipped first moment
    for normal distributions which is computed using \code{dnorm} and \code{pnorm}. }

  \item{object = "Exp":}{ clipped first moment
    for exponential distributions which is computed using \code{pexp}. }

  \item{object = "Chisq":}{ clipped first moment
    for \eqn{\chi^2}{Chi^2} distributions which is computed using \code{pchisq}. }
}}
%\references{ ~put references to the literature/web site here ~ }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{distrExIntegrate}}, \code{\link{m2df}}, \code{\link{E}}}
\examples{
# standard normal distribution
N1 <- Norm()
m1df(N1, 0)

# Poisson distribution
P1 <- Pois(lambda=2)
m1df(P1, 3)

# absolutely continuous distribution
D1 <- Norm() + Exp() # convolution
m1df(D1, 2)
m1df(D1, Inf)
E(D1)
}
\concept{functional}
\concept{truncated moment}
\concept{moment}
\keyword{distribution}
\keyword{methods}
