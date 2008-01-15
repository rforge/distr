\name{TotalVarDist}
\alias{TotalVarDist}
\alias{TotalVarDist-methods}
\alias{TotalVarDist,AbscontDistribution,AbscontDistribution-method}
\alias{TotalVarDist,AbscontDistribution,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,AbscontDistribution-method}

\title{Generic function for the computation of the total variation distance of two distributions}
\description{
  Generic function for the computation of the total variation distance \eqn{d_v}
  of two distributions \eqn{P} and \eqn{Q} where the distributions may be 
  defined for an arbitrary sample space \eqn{(\Omega,{\cal A})}{(Omega, A)}.
  The total variation distance is defined as
  \deqn{d_v(P,Q)=\sup_{B\in{\cal A}}|P(B)-Q(B)|}{d_v(P,Q)=\sup\{|P(B)-Q(B)| | B in A\}}
}
\usage{
TotalVarDist(e1, e2, ...)
\S4method{TotalVarDist}{AbscontDistribution,AbscontDistribution}(e1,e2)
\S4method{TotalVarDist}{AbscontDistribution,DiscreteDistribution}(e1,e2)
\S4method{TotalVarDist}{DiscreteDistribution,AbscontDistribution}(e1,e2)
\S4method{TotalVarDist}{DiscreteDistribution,DiscreteDistribution}(e1,e2)
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
  \item{total.variation.distance}{ total variation distance of \code{e1} and \code{e2} }
}
\section{Methods}{
\describe{
  \item{e1 = "AbscontDistribution", e2 = "AbscontDistribution":}{
    total variation distance of two absolutely continuous 
    univariate distributions which is computed using \code{distrExIntegrate}.
  }

  \item{e1 = "AbscontDistribution", e2 = "DiscreteDistribution":}{
    total variation distance of absolutely continuous and discrete
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).}

  \item{e1 = "DiscreteDistribution", e2 = "DiscreteDistribution":}{
    total variation distance of two discrete univariate distributions
    which is computed using \code{support} and \code{sum}.
  }

  \item{e1 = "DiscreteDistribution", e2 = "AbscontDistribution":}{
    total variation distance of discrete and absolutely continuous 
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).}
}}
\references{Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{TotalVarDist-methods}}, \code{\link{ContaminationSize}}, 
    \code{\link{KolmogorovDist}}, \code{\link{HellingerDist}}, 
    \code{\link[distr]{Distribution-class}}}
\examples{
TotalVarDist(Norm(), Gumbel())
TotalVarDist(Norm(), Td(10))
TotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
TotalVarDist(Pois(10), Binom(size = 20)) 
}
\concept{distance}
\keyword{distribution}
