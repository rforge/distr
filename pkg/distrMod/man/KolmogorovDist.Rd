\name{KolmogorovDist}
\alias{KolmogorovDist}
\alias{KolmogorovDist,numeric,UnivariateDistribution-method}
\alias{KolmogorovDist,UnivariateDistribution,numeric-method}

\title{Methods for the computation of the Kolmogorov distance between (empirical) data and distributions}
\description{
  Generic function for the computation of the Kolmogorov distance between (empirical) 
  data and distributions.
}
\usage{
\S4method{KolmogorovDist}{numeric,UnivariateDistribution}(e1, e2)
\S4method{KolmogorovDist}{UnivariateDistribution,numeric}(e1, e2)
}
\arguments{
  \item{e1}{ object of class \code{"Distribution"} or class \code{"numeric"} }
  \item{e2}{ object of class \code{"Distribution"} or class \code{"numeric"} }
}
%\details{}
\value{
  Kolmogorov distance of \code{e1} and \code{e2}.
}
\section{Methods}{
\describe{
  \item{e1 = "numeric", e2 = "UnivariateDistribution":}{
    Kolmogorov distance between (empirical) data and a univariate 
    distribution. The computation is based on \code{ks.test}.
  }
  \item{e1 = "UnivariateDistribution", e2 = "numeric":}{
    Kolmogorov distance between (empirical) data and a univariate 
    distribution. The computation is based on \code{ks.test}.
  }
}}
\references{
    Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.

    Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link[distrEx]{KolmogorovDist}}, \code{\link[stats]{ks.test}}}
\examples{
KolmogorovDist(Norm(), rnorm(100))
KolmogorovDist((rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5), Norm())
KolmogorovDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))
}
\concept{distance}
\keyword{distribution}
