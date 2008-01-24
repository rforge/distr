\name{HellingerDist}
\alias{HellingerDist}
\alias{HellingerDist-methods}
\alias{HellingerDist,AbscontDistribution,AbscontDistribution-method}
\alias{HellingerDist,AbscontDistribution,DiscreteDistribution-method}
\alias{HellingerDist,DiscreteDistribution,DiscreteDistribution-method}
\alias{HellingerDist,DiscreteDistribution,AbscontDistribution-method}

\title{Generic function for the computation of the Hellinger distance of two distributions}
\description{
  Generic function for the computation of the Hellinger distance \eqn{d_h}
  of two distributions \eqn{P} and \eqn{Q} which may be defined for an arbitrary 
  sample space \eqn{(\Omega,{\cal A})}{(Omega, A)}. The Hellinger distance is defined as
  \deqn{d_h(P,Q)=\frac{1}{2}\int|\sqrt{dP}\,-\sqrt{dQ}\,|^2}{0.5 \int |\sqrt{dP}-\sqrt{dQ}|^2}
  where \eqn{\sqrt{dP}}, respectively \eqn{\sqrt{dQ}} denotes the square root 
  of the densities.
}
\usage{
HellingerDist(e1, e2, ...)
\S4method{HellingerDist}{AbscontDistribution,AbscontDistribution}(e1,e2)
\S4method{HellingerDist}{AbscontDistribution,DiscreteDistribution}(e1,e2)
\S4method{HellingerDist}{DiscreteDistribution,AbscontDistribution}(e1,e2)
\S4method{HellingerDist}{DiscreteDistribution,DiscreteDistribution}(e1,e2)
}
\arguments{
  \item{e1}{ object of class \code{"Distribution"} }
  \item{e2}{ object of class \code{"Distribution"} }
  \item{\dots}{ further arguments to be used in particular methods (not in package \pkg{distrEx})}
}
%\details{}
\value{
  A list containing the following components:
  \item{e1}{ object of class \code{"Distribution"}; distribution 1  }
  \item{e2}{ object of class \code{"Distribution"}; distribution 2}
  \item{Hellinger.distance}{ Hellinger distance of \code{e1} and \code{e2}}
}
\section{Methods}{
\describe{
  \item{e1 = "AbscontDistribution", e2 = "AbscontDistribution":}{
    Hellinger distance of two absolutely continuous 
    univariate distributions which is computed using \code{distrExintegrate}.
  }

  \item{e1 = "AbscontDistribution", e2 = "DiscreteDistribution":}{
    Hellinger distance of absolutely continuous and discrete
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).}

  \item{e1 = "DiscreteDistribution", e2 = "DiscreteDistribution":}{
    Hellinger distance of two discrete univariate distributions
    which is computed using \code{support} and \code{sum}.
  }

  \item{e1 = "DiscreteDistribution", e2 = "AbscontDistribution":}{
    \crHellinger distance of discrete and absolutely continuous 
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).}
}}
\references{Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{distrExIntegrate}}, \code{\link{ContaminationSize}}, 
    \code{\link{TotalVarDist}}, \code{\link{KolmogorovDist}}, 
    \code{\link[distr]{Distribution-class}}}
\examples{
HellingerDist(Norm(), Gumbel())
HellingerDist(Norm(), Td(10))
HellingerDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
HellingerDist(Pois(10), Binom(size = 20)) 
}
\concept{distance}
\keyword{distribution}
