\name{E}
\alias{E}
\alias{E-methods}
\alias{E,UnivariateDistribution,missing,missing-method}
\alias{E,AbscontDistribution,missing,missing-method}
\alias{E,DiscreteDistribution,missing,missing-method}
\alias{E,AffLinDistribution,missing,missing-method}
\alias{E,AffLinAbscontDistribution,missing,missing-method}
\alias{E,AffLinDiscreteDistribution,missing,missing-method}
\alias{E,AffLinLatticeDistribution,missing,missing-method}
\alias{E,MultivariateDistribution,missing,missing-method}
\alias{E,DiscreteMVDistribution,missing,missing-method}
\alias{E,UnivariateDistribution,function,missing-method}
\alias{E,AbscontDistribution,function,missing-method}
\alias{E,DiscreteDistribution,function,missing-method}
\alias{E,MultivariateDistribution,function,missing-method}
\alias{E,DiscreteMVDistribution,function,missing-method}
\alias{E,UnivariateCondDistribution,missing,numeric-method}
\alias{E,AbscontCondDistribution,missing,numeric-method}
\alias{E,DiscreteCondDistribution,missing,numeric-method}
\alias{E,UnivariateCondDistribution,function,numeric-method}
\alias{E,AbscontCondDistribution,function,numeric-method}
\alias{E,DiscreteCondDistribution,function,numeric-method}
\alias{E,Beta,missing,missing-method}
\alias{E,Binom,missing,missing-method}
\alias{E,Cauchy,missing,missing-method}
\alias{E,Chisq,missing,missing-method}
\alias{E,Dirac,missing,missing-method}
\alias{E,DExp,missing,missing-method}
\alias{E,Exp,missing,missing-method}
\alias{E,Fd,missing,missing-method}
\alias{E,Gammad,missing,missing-method}
\alias{E,Geom,missing,missing-method}
\alias{E,Hyper,missing,missing-method}
\alias{E,Logis,missing,missing-method}
\alias{E,Lnorm,missing,missing-method}
\alias{E,Nbinom,missing,missing-method}
\alias{E,Norm,missing,missing-method}
\alias{E,Pois,missing,missing-method}
\alias{E,Td,missing,missing-method}
\alias{E,Unif,missing,missing-method}
\alias{E,Weibull,missing,missing-method}

\title{Generic Function for the Computation of (Conditional) Expectations}
\description{
  Generic function for the computation of (conditional) expectations.
}
\usage{
E(object, fun, cond, ...)

\S4method{E}{UnivariateDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{AbscontDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{DiscreteDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{AffLinDistribution,missing,missing}(object)

\S4method{E}{MultivariateDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{DiscreteMVDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{AbscontCondDistribution,missing,numeric}(object, cond, useApply = TRUE)

\S4method{E}{DiscreteCondDistribution,missing,numeric}(object, cond, useApply = TRUE)

\S4method{E}{UnivariateCondDistribution,function,numeric}(object, fun, cond, withCond = FALSE, useApply = TRUE, ...)

\S4method{E}{AbscontCondDistribution,function,numeric}(object, fun, cond, withCond = FALSE, useApply = TRUE, ...)

\S4method{E}{DiscreteCondDistribution,function,numeric}(object, fun, cond, withCond = FALSE, useApply = TRUE, ...)

\S4method{E}{DiscreteCondDistribution,function,numeric}(object, fun, cond, withCond = FALSE, useApply = TRUE, ...)

\S4method{E}{Beta,missing,missing}(object, fun, cond)
\S4method{E}{Binom,missing,missing}(object, fun, cond)
\S4method{E}{Cauchy,missing,missing}(object, fun, cond)
\S4method{E}{Chisq,missing,missing}(object, fun, cond)
\S4method{E}{Dirac,missing,missing}(object, fun, cond)
\S4method{E}{DExp,missing,missing}(object, fun, cond)
\S4method{E}{Exp,missing,missing}(object, fun, cond)
\S4method{E}{Fd,missing,missing}(object, fun, cond)
\S4method{E}{Gammad,missing,missing}(object, fun, cond)
\S4method{E}{Geom,missing,missing}(object, fun, cond)
\S4method{E}{Hyper,missing,missing}(object, fun, cond)
\S4method{E}{Logis,missing,missing}(object, fun, cond)
\S4method{E}{Lnorm,missing,missing}(object, fun, cond)
\S4method{E}{Nbinom,missing,missing}(object, fun, cond)
\S4method{E}{Norm,missing,missing}(object, fun, cond)
\S4method{E}{Pois,missing,missing}(object, fun, cond)
\S4method{E}{Unif,missing,missing}(object, fun, cond)
\S4method{E}{Td,missing,missing}(object, fun, cond)
\S4method{E}{Weibull,missing,missing}(object, fun, cond)

}
\arguments{
  \item{object}{ object of class \code{"Distribution"}}
  \item{fun}{ if missing the (conditional) expectation is computed
    else the (conditional) expection of \code{fun} is computed. }
  \item{cond}{ if not missing the conditional expectation 
    given \code{cond} is computed. }
  \item{\dots}{ additional arguments to \code{fun} }
  \item{useApply}{ logical: should \code{sapply}, respectively \code{apply} 
    be used to evaluate \code{fun}. }
  \item{withCond}{ logical: is \code{cond} in the argument list of \code{fun}. }
}
\details{The precision of the computations can be controlled via 
  certain global options; cf. \code{\link{distrExOptions}}. }
\value{
  The (conditional) expectation is computed.
}
\section{Methods}{
\describe{
  \item{object = "UnivariateDistribution", fun = "missing", cond = "missing":}{ 
    expectation of univariate distributions using crude Monte-Carlo integration. }

  \item{object = "AbscontDistribution", fun = "missing", cond = "missing":}{ 
    expectation of absolutely continuous univariate distributions
    using \code{distrExIntegrate}. }

  \item{object = "DiscreteDistribution", fun = "missing", cond = "missing":}{  
    expectation of discrete univariate distributions using \code{support}
    and \code{sum}.}

  \item{object = "MultivariateDistribution", fun = "missing", cond = "missing":}{
    expectation of multivariate distributions using crude Monte-Carlo integration. }

  \item{object = "DiscreteMVDistribution", fun = "missing", cond = "missing":}{
    expectation of discrete multivariate distributions. The computation is based
    on \code{support} and \code{sum}.}

  \item{object = "UnivariateDistribution", fun = "function", cond = "missing":}{ 
    expectation of \code{fun} under univariate distributions using 
    crude Monte-Carlo integration. }

  \item{object = "AbscontDistribution", fun = "function", cond = "missing":}{ 
    expectation of \code{fun} under absolutely continuous 
    univariate distributions using \code{distrExIntegrate}. }

  \item{object = "DiscreteDistribution", fun = "function", cond = "missing":}{  
    expectation of \code{fun} under discrete univariate 
    distributions using \code{support} and \code{sum}. }

  \item{object = "MultivariateDistribution", fun = "function", cond = "missing":}{
    expectation of multivariate distributions using crude Monte-Carlo integration. }

  \item{object = "DiscreteMVDistribution", fun = "function", cond = "missing":}{
    expectation of \code{fun} under discrete multivariate 
    distributions. The computation is based on \code{support} and \code{sum}. }  

  \item{object = "UnivariateCondDistribution", fun = "missing", cond = "numeric":}{
    conditional expectation for univariate conditional distributions given \code{cond}. 
    The integral is computed using crude Monte-Carlo integration. }

  \item{object = "AbscontCondDistribution", fun = "missing", cond = "numeric":}{
    conditional expectation for absolutely continuous, univariate 
    conditional distributions given \code{cond}. The computation
    is based on \code{distrExIntegrate}. }

  \item{object = "DiscreteCondDistribution", fun = "missing", cond = "numeric":}{ 
    conditional expectation for discrete, univariate conditional 
    distributions given \code{cond}. The computation is based 
    on \code{support} and \code{sum}. }

  \item{object = "UnivariateCondDistribution", fun = "function", cond = "numeric":}{
    conditional expectation of \code{fun} under univariate conditional distributions 
    given \code{cond}. The integral is computed using crude Monte-Carlo integration. }

  \item{object = "AbscontCondDistribution", fun = "function", cond = "numeric":}{
    conditional expectation of \code{fun} under absolutely continuous, 
    univariate conditional distributions given \code{cond}. The
    computation is based on \code{distrExIntegrate}. }

  \item{object = "DiscreteCondDistribution", fun = "function", cond = "numeric":}{ 
    conditional expectation of \code{fun} under discrete, univariate 
    conditional distributions given \code{cond}. The computation is
    based on \code{support} and \code{sum}. }

  \item{object = "Beta", fun = "missing", cond = "missing":}{ 
    for noncentrality 0 exact evaluation using explicit expressions.}
  \item{object = "Binom", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Cauchy", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Chisq", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Dirac", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "DExp", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Exp", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Fd", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Gammad", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Geom", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Hyper", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Logis", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Lnorm", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Nbinom", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Norm", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Pois", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Unif", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Td", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
  \item{object = "Weibull", fun = "missing", cond = "missing":}{ 
    exact evaluation using explicit expressions.}
}}
%\references{ ~put references to the literature/web site here ~ }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de} and Peter Ruckdeschel \email{peter.ruckdeschel@uni-bayreuth.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{distrExIntegrate}}, \code{\link{m1df}}, \code{\link{m2df}},
    \code{\link[distr]{Distribution-class}}}
\examples{
# mean of Exp(1) distribution
E <- Exp() 

E(E) ## uses explicit terms
E(as(E,"AbscontDistribution")) ## uses numerical integration
E(as(E,"UnivariateDistribution")) ## uses simulations
E(E, fun = function(x){2*x^2}) ## uses simulations

# the same operator for discrete distributions:
P <- Pois(lambda=2)

E(P) ## uses explicit terms
E(as(P,"DiscreteDistribution")) ## uses sums
E(as(P,"UnivariateDistribution")) ## uses simulations
E(P, fun = function(x){2*x^2}) ## uses simulations


# second moment of N(1,4)
E(Norm(mean=1, sd=2), fun = function(x){x^2})
E(Norm(mean=1, sd=2), fun = function(x){x^2}, useApply = FALSE)

# conditional distribution of a linear model
D1 <- LMCondDistribution(theta = 1) 
E(D1, cond = 1)
E(Norm(mean=1))
E(D1, function(x){x^2}, cond = 1)
E(Norm(mean=1), fun = function(x){x^2})
E(D1, function(x, cond){cond*x^2}, cond = 2, withCond = TRUE, useApply = FALSE)
E(Norm(mean=2), function(x){2*x^2})
}
\concept{expectation}
\concept{conditional expectation}
\concept{integration}
\keyword{methods}
\keyword{distribution}
\concept{functional}
\concept{moment}
