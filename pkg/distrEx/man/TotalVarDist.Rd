\name{TotalVarDist}
\alias{TotalVarDist}
\alias{TotalVarDist-methods}
\alias{TotalVarDist,AbscontDistribution,AbscontDistribution-method}
\alias{TotalVarDist,AbscontDistribution,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,AbscontDistribution-method}
\alias{TotalVarDist,numeric,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,numeric-method}
\alias{TotalVarDist,numeric,AbscontDistribution-method}
\alias{TotalVarDist,AbscontDistribution,numeric-method}

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
\S4method{TotalVarDist}{numeric,DiscreteDistribution}(e1, e2)
\S4method{TotalVarDist}{DiscreteDistribution,numeric}(e1, e2)
\S4method{TotalVarDist}{numeric,AbscontDistribution}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrModOption("nDiscretize"), low.discr = getLow(e2),
            up.discr = getUp(e2), h.smooth = getdistrModOption("hSmooth"))
\S4method{TotalVarDist}{AbscontDistribution,numeric}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrModOption("nDiscretize"), low.discr = getLow(e2),
            up.discr = getUp(e2), h.smooth = getdistrModOption("hSmooth"))
}
\arguments{
  \item{e1}{ object of class \code{"Distribution"} or \code{"numeric"} }
  \item{e2}{ object of class \code{"Distribution"} or \code{"numeric"} }
  \item{asis.smooth.discretize}{ possible methods are \code{"asis"}, 
    \code{"smooth"} and \code{"discretize"}. Default is \code{"discretize"}.}
  \item{n.discr}{ if \code{asis.smooth.discretize} is equal to 
    \code{"discretize"} one has to specify the number of lattice points 
    used to discretize the abs. cont. distribution.}
  \item{low.discr}{ if \code{asis.smooth.discretize} is equal to 
    \code{"discretize"} one has to specify the lower end point of the lattice 
    used to discretize the abs. cont. distribution.}
  \item{up.discr}{ if \code{asis.smooth.discretize} is equal to 
    \code{"discretize"} one has to specify the upper end point of the lattice 
    used to discretize the abs. cont. distribution.}
  \item{h.smooth}{ if \code{asis.smooth.discretize} is equal to 
    \code{"smooth"} -- i.e., the empirical distribution of the provided 
    data should be smoothed -- one has to specify this parameter.}
  \item{\dots}{ further arguments to be used in particular methods (not in package \pkg{distrEx})}
}
\details{
  In case we want to compute the total variation distance between (empirical) data
  and an abs. cont. distribution, we can specify the parameter \code{asis.smooth.discretize}
  to avoid trivial distances (distance = 1).

  Using \code{asis.smooth.discretize = "discretize"}, which is the default,
  leads to a discretization of the provided abs. cont. distribution and 
  the distance is computed between the provided data and the discretized
  distribution.

  Using \code{asis.smooth.discretize = "smooth"} causes smoothing of the
  empirical distribution of the provided data. This is, the empirical
  data is convoluted with the normal distribution \code{Norm(mean = 0, sd = h.smooth)}
  which leads to an abs. cont. distribution. Afterwards the distance 
  between the smoothed empirical distribution and the provided abs. cont.
  distribution is computed.
}
\value{ Total variation distance of \code{e1} and \code{e2} }
\section{Methods}{
\describe{
  \item{e1 = "AbscontDistribution", e2 = "AbscontDistribution":}{
    total variation distance of two absolutely continuous 
    univariate distributions which is computed using \code{distrExIntegrate}.
  }
  \item{e1 = "AbscontDistribution", e2 = "DiscreteDistribution":}{
    total variation distance of absolutely continuous and discrete
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).
  }
  \item{e1 = "DiscreteDistribution", e2 = "DiscreteDistribution":}{
    total variation distance of two discrete univariate distributions
    which is computed using \code{support} and \code{sum}.
  }
  \item{e1 = "DiscreteDistribution", e2 = "AbscontDistribution":}{
    total variation distance of discrete and absolutely continuous 
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).
  }
  \item{e1 = "numeric", e2 = "DiscreteDistribution":}{
    Total variation distance between (empirical) data and a discrete 
    distribution.
  }
  \item{e1 = "DiscreteDistribution", e2 = "numeric":}{
    Total variation distance between (empirical) data and a discrete 
    distribution.
  }
  \item{e1 = "numeric", e2 = "AbscontDistribution":}{
    Total variation distance between (empirical) data and an abs. cont. 
    distribution.
  }
  \item{e1 = "AbscontDistribution", e1 = "numeric":}{
    Total variation distance between (empirical) data and an abs. cont. 
    distribution.
  }
}}
\references{
    Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.

    Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{TotalVarDist-methods}}, \code{\link{ContaminationSize}}, 
    \code{\link{KolmogorovDist}}, \code{\link{HellingerDist}}, 
    \code{\link[distr]{Distribution-class}}}
\examples{
TotalVarDist(Norm(), Gumbel())
TotalVarDist(Norm(), Td(10))
TotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
TotalVarDist(Pois(10), Binom(size = 20)) 

x <- rnorm(100)
TotalVarDist(Norm(), x)
TotalVarDist(x, Norm(), asis.smooth.discretize = "smooth")

y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
TotalVarDist(y, Norm())
TotalVarDist(y, Norm(), asis.smooth.discretize = "smooth")

TotalVarDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))
}
\concept{distance}
\keyword{distribution}
