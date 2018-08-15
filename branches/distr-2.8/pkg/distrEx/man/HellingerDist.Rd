\name{HellingerDist}
\alias{HellingerDist}
\alias{HellingerDist-methods}
\alias{HellingerDist,AbscontDistribution,AbscontDistribution-method}
\alias{HellingerDist,AbscontDistribution,DiscreteDistribution-method}
\alias{HellingerDist,DiscreteDistribution,DiscreteDistribution-method}
\alias{HellingerDist,DiscreteMVDistribution,DiscreteMVDistribution-method}
\alias{HellingerDist,DiscreteDistribution,AbscontDistribution-method}
\alias{HellingerDist,LatticeDistribution,DiscreteDistribution-method}
\alias{HellingerDist,DiscreteDistribution,LatticeDistribution-method}
\alias{HellingerDist,LatticeDistribution,LatticeDistribution-method}
\alias{HellingerDist,numeric,DiscreteDistribution-method}
\alias{HellingerDist,DiscreteDistribution,numeric-method}
\alias{HellingerDist,numeric,AbscontDistribution-method}
\alias{HellingerDist,AbscontDistribution,numeric-method}
\alias{HellingerDist,AcDcLcDistribution,AcDcLcDistribution-method}

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
\S4method{HellingerDist}{AbscontDistribution,AbscontDistribution}(e1,e2, 
                        rel.tol=.Machine$double.eps^0.3, 
                        TruncQuantile = getdistrOption("TruncQuantile"), 
                        IQR.fac = 15, ..., diagnostic = FALSE)
\S4method{HellingerDist}{AbscontDistribution,DiscreteDistribution}(e1,e2, ...)
\S4method{HellingerDist}{DiscreteDistribution,AbscontDistribution}(e1,e2, ...)
\S4method{HellingerDist}{DiscreteDistribution,DiscreteDistribution}(e1,e2, ...)
\S4method{HellingerDist}{numeric,DiscreteDistribution}(e1, e2, ...)
\S4method{HellingerDist}{DiscreteDistribution,numeric}(e1, e2, ...)
\S4method{HellingerDist}{numeric,AbscontDistribution}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrExOption("nDiscretize"), low.discr = getLow(e2),
            up.discr = getUp(e2), h.smooth = getdistrExOption("hSmooth"),
                        rel.tol=.Machine$double.eps^0.3, 
                        TruncQuantile = getdistrOption("TruncQuantile"), 
                        IQR.fac = 15, ..., diagnostic = FALSE)
\S4method{HellingerDist}{AbscontDistribution,numeric}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrExOption("nDiscretize"), low.discr = getLow(e1),
            up.discr = getUp(e1), h.smooth = getdistrExOption("hSmooth"), 
                        rel.tol=.Machine$double.eps^0.3, 
                        TruncQuantile = getdistrOption("TruncQuantile"), 
                        IQR.fac = 15, ..., diagnostic = FALSE)
\S4method{HellingerDist}{AcDcLcDistribution,AcDcLcDistribution}(e1,e2, 
                        rel.tol=.Machine$double.eps^0.3, 
                        TruncQuantile = getdistrOption("TruncQuantile"), 
                        IQR.fac = 15, ..., diagnostic = FALSE)
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
  \item{rel.tol}{ relative accuracy requested in integration }
  \item{TruncQuantile}{Quantile the quantile based integration bounds (see details)}
  \item{IQR.fac}{Factor for the scale based integration bounds (see details)}
  \item{\dots}{ further arguments to be used in particular methods -- (in package \pkg{distrEx}: just
     used for distributions with a.c. parts, where it is used to pass on arguments
     to \code{distrExIntegrate}). }
  \item{diagnostic}{ logical; if \code{TRUE}, the return value obtains
    an attribute \code{"diagnostic"} with diagnostic information on the
    integration, i.e., a list with entries \code{method} (\code{"integrate"}
    or \code{"GLIntegrate"}), \code{call}, \code{result} (the complete return
    value of the method),  \code{args} (the args with which the
    method was called), and \code{time} (the time to compute the integral). }
}
\details{
  For distances between absolutely continuous distributions, we use numerical
  integration; to determine sensible bounds we proceed as follows:
  by means of \code{min(getLow(e1,eps=TruncQuantile),getLow(e2,eps=TruncQuantile))}, 
  \code{max(getUp(e1,eps=TruncQuantile),getUp(e2,eps=TruncQuantile))} we determine
  quantile based bounds \code{c(low.0,up.0)}, and by means of
  \code{s1 <- max(IQR(e1),IQR(e2));} \code{m1<- median(e1);} 
  \code{m2 <- median(e2)}
  and \code{low.1 <- min(m1,m2)-s1*IQR.fac}, \code{up.1 <- max(m1,m2)+s1*IQR.fac}
  we determine scale based bounds; these are combined by
  \code{low <- max(low.0,low.1)}, \code{up <- max(up.0,up1)}.

  In case we want to compute the Hellinger distance between (empirical) data
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
\value{ Hellinger distance of \code{e1} and \code{e2} }
\section{Methods}{
\describe{
  \item{e1 = "AbscontDistribution", e2 = "AbscontDistribution":}{
    Hellinger distance of two absolutely continuous 
    univariate distributions which is computed using \code{distrExintegrate}.
  }
  \item{e1 = "AbscontDistribution", e2 = "DiscreteDistribution":}{
    Hellinger distance of absolutely continuous and discrete
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).
  }
  \item{e1 = "DiscreteDistribution", e2 = "DiscreteDistribution":}{
    Hellinger distance of two discrete univariate distributions
    which is computed using \code{support} and \code{sum}.
  }
  \item{e1 = "DiscreteDistribution", e2 = "AbscontDistribution":}{
    Hellinger distance of discrete and absolutely continuous 
    univariate distributions (are mutually singular; i.e., 
    have distance \code{=1}).
  }
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
  \item{e1 = "AcDcLcDistribution", e2 = "AcDcLcDistribution":}{
    Hellinger distance of mixed discrete and absolutely continuous 
    univariate distributions.
  }
}}
\references{
    Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.

    Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{distrExIntegrate}}, \code{\link{ContaminationSize}}, 
    \code{\link{TotalVarDist}}, \code{\link{KolmogorovDist}}, 
    \code{\link[distr]{Distribution-class}}}
\examples{
HellingerDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)))
HellingerDist(Norm(), Td(10))
HellingerDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
HellingerDist(Pois(10), Binom(size = 20)) 

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
