\name{vonMisesDist}
\alias{vonMisesDist}
\alias{vonMisesDist-methods}
\alias{vonMisesDist,UnivariateDistribution,UnivariateDistribution-method}
\alias{vonMisesDist,numeric,UnivariateDistribution-method}

\title{Generic function for the computation of the vonMises distance of two distributions}
\description{
  Generic function for the computation of the von Mises distance \eqn{d_\mu}{d_{mu}}
  of two distributions \eqn{P} and \eqn{Q} where the distributions are defined 
  on a finite-dimensional Euclidean space \eqn{(\R^m,{\cal B}^m)}{(R^m, B^m)}
  with \eqn{ {\cal B}^m }{B^m} the Borel-\eqn{\sigma}{sigma}-algebra on \eqn{R^m}.
  The von Mises distance is defined as
  \deqn{d_\mu(P,Q)^2=\int\,(P(\{y\in\R^m\,|\,y\le x\})-Q(\{y\in\R^m\,|\,y\le x\}))^2\,\mu(dx)}{
  d_{mu}(P,Q)^2=\int (P(\{y in R^m | y <= x\})-Q(\{y in R^m | y <= x\}))^2 mu(dx)}
  where \eqn{\le}{<=} is coordinatewise on \eqn{\R^m}{R^m}.
}
\usage{
vonMisesDist(e1, e2, ...)
\S4method{vonMisesDist}{UnivariateDistribution,UnivariateDistribution}(e1, e2, mu = e2, useApply = FALSE, ...)
\S4method{vonMisesDist}{numeric,UnivariateDistribution}(e1, e2, mu = e2, ...)
}
\arguments{
  \item{e1}{ object of class \code{"Distribution"} or class \code{"numeric"} }
  \item{e2}{ object of class \code{"Distribution"} }
  \item{\dots}{ further arguments to be used e.g. by \code{E()}}
  \item{useApply}{ logical; to be passed to \code{\link{E}()}}
  \item{mu}{ object of class \code{"Distribution"}; integration measure; defaulting to \code{e2} }
}
%\details{}
\value{ von Mises distance of \code{e1} and \code{e2} }
\section{Methods}{
\describe{
  \item{e1 = "UnivariateDistribution", e2 = "UnivariateDistribution":}{
    von Mises distance of two univariate distributions.
  }
  \item{e1 = "numeric", e2 = "UnivariateDistribution":}{
    von Mises distance between the empirical formed from a data set (e1) and a 
    univariate distribution. 
  }
}}
\references{
    Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{ContaminationSize}}, \code{\link{TotalVarDist}}, 
    \code{\link{HellingerDist}}, \code{\link{KolmogorovDist}},
    \code{\link[distr]{Distribution-class}}}
\examples{
vonMisesDist(Norm(), Gumbel())
vonMisesDist(Norm(), Gumbel(), mu = Norm())
vonMisesDist(Norm(), Td(10))
vonMisesDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100))
vonMisesDist(Pois(10), Binom(size = 20)) 
vonMisesDist(rnorm(100),Norm())
vonMisesDist((rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5), Norm())
vonMisesDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))
vonMisesDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5), mu = Pois())
}
\concept{distance}
\keyword{distribution}
