\name{HellingerDist}
\alias{HellingerDist}
\alias{HellingerDist,numeric,DiscreteDistribution-method}
\alias{HellingerDist,DiscreteDistribution,numeric-method}
\alias{HellingerDist,numeric,AbscontDistribution-method}
\alias{HellingerDist,AbscontDistribution,numeric-method}

\title{Methods for the computation of the Hellinger distance between (empirical) data and distributions}
\description{
  Generic function for the computation of the Hellinger distance between (empirical) 
  data and distributions.
}
\usage{
\S4method{HellingerDist}{numeric,DiscreteDistribution}(e1, e2)
\S4method{HellingerDist}{DiscreteDistribution,numeric}(e1, e2)
\S4method{HellingerDist}{numeric,AbscontDistribution}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrModOption("nDiscretize"), low.discr = getLow(e2),
            up.discr = getUp(e2), h.smooth = getdistrModOption("hSmooth"))
\S4method{HellingerDist}{AbscontDistribution,numeric}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrModOption("nDiscretize"), low.discr = getLow(e2),
            up.discr = getUp(e2), h.smooth = getdistrModOption("hSmooth"))
}
\arguments{
  \item{e1}{ object of class \code{"Distribution"} or class \code{"numeric"} }
  \item{e2}{ object of class \code{"Distribution"} or class \code{"numeric"} }
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
}
\details{
  In case we want compute the Hellinger distance between (empirical) data
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
\value{
  Hellinger distance of \code{e1} and \code{e2}.
}
\section{Methods}{
\describe{
  \item{e1 = "numeric", e2 = "DiscreteDistribution":}{
    Hellinger distance between (empirical) data and a discrete 
    distribution.
  }
  \item{e1 = "DiscreteDistribution", e2 = "numeric":}{
    Hellinger distance between (empirical) data and a discrete
    distribution.
  }
  \item{e1 = "numeric", e2 = "AbscontDistribution":}{
    Hellinger distance between (empirical) data and an abs. cont. 
    distribution.
  }
  \item{e1 = "AbscontDistribution", e1 = "numeric":}{
    Hellinger distance between (empirical) data and an abs. cont. 
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
\seealso{\code{\link[distrEx]{HellingerDist}}}
\examples{
x <- rnorm(100)
HellingerDist(Norm(), x)
HellingerDist(x, Norm(), asis.smooth.discretize = "smooth")

y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
HellingerDist(y, Norm())
HellingerDist(y, Norm(), asis.smooth.discretize = "smooth")

HellingerDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))
}
\concept{distance}
\keyword{distribution}
