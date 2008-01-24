\name{KolmogorovDist}
\alias{KolmogorovDist}
\alias{KolmogorovDist-methods}
\alias{KolmogorovDist,AbscontDistribution,AbscontDistribution-method}
\alias{KolmogorovDist,AbscontDistribution,DiscreteDistribution-method}
\alias{KolmogorovDist,DiscreteDistribution,DiscreteDistribution-method}
\alias{KolmogorovDist,DiscreteDistribution,AbscontDistribution-method}

\title{Generic function for the computation of the Kolmogorov distance of two distributions}
\description{
  Generic function for the computation of the Kolmogorov distance \eqn{d_\kappa}{d_k}
  of two distributions \eqn{P} and \eqn{Q} where the distributions are defined 
  on a finite-dimensional Euclidean space \eqn{(\R^m,{\cal B}^m)}{(R^m, B^m)}
  with \eqn{ {\cal B}^m }{B^m} the Borel-\eqn{\sigma}{sigma}-algebra on \eqn{R^m}.
  The Kolmogorov distance is defined as
  \deqn{d_\kappa(P,Q)=\sup\{|P(\{y\in\R^m\,|\,y\le x\})-Q(\{y\in\R^m\,|\,y\le x\})| | x\in\R^m\}}{
        d_k(P,Q)=\sup\{|P(\{y in R^m | y <= x\})-Q(\{y in R^m | y <= x\})| | x in R^m\}}
  where \eqn{\le} is coordinatewise on \eqn{\R^m}{R^m}.
}
\usage{
KolmogorovDist(e1, e2, ...)
\S4method{KolmogorovDist}{AbscontDistribution,AbscontDistribution}(e1,e2)
\S4method{KolmogorovDist}{AbscontDistribution,DiscreteDistribution}(e1,e2)
\S4method{KolmogorovDist}{DiscreteDistribution,AbscontDistribution}(e1,e2)
\S4method{KolmogorovDist}{DiscreteDistribution,DiscreteDistribution}(e1,e2)
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
  \item{Kolmogorov.distance}{ Kolmogorov distance of \code{e1} and \code{e2}}
}
\section{Methods}{
\describe{
  \item{e1 = "AbscontDistribution", e2 = "AbscontDistribution":}{
    Kolmogorov distance of two absolutely continuous 
    univariate distributions which is computed using a
    union of a (pseudo-)random and a deterministic grid.
  }
  \item{e1 = "DiscreteDistribution", e2 = "DiscreteDistribution":}{
    Kolmogorov distance of two discrete univariate distributions. 
    The distance is attained at some point of the union of the supports 
    of \code{e1} and \code{e2}.
  }
  \item{e1 = "AbscontDistribution", e2 = "DiscreteDistribution":}{
    Kolmogorov distance of absolutely continuous and discrete
    univariate distributions. It is computed using a union of 
    a (pseudo-)random and a deterministic grid in combination
    with the support of \code{e2}.
  }
  \item{e1 = "DiscreteDistribution", e2 = "AbscontDistribution":}{
    Kolmogorov distance of discrete and absolutely continuous
    univariate distributions. It is computed using a union of 
    a (pseudo-)random and a deterministic grid in combination
    with the support of \code{e1}.
  }
}}
\references{
    Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.
    
    Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{ContaminationSize}}, \code{\link{TotalVarDist}}, 
    \code{\link{HellingerDist}}, \code{\link[distr]{Distribution-class}}}
\examples{
KolmogorovDist(Norm(), Gumbel())
KolmogorovDist(Norm(), Td(10))
KolmogorovDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100))
KolmogorovDist(Pois(10), Binom(size = 20)) 
}
\concept{distance}
\keyword{distribution}
