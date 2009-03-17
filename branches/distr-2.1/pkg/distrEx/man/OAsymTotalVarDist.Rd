\name{OAsymTotalVarDist}
\alias{OAsymTotalVarDist}
\alias{OAsymTotalVarDist-methods}
\alias{OAsymTotalVarDist,AbscontDistribution,AbscontDistribution-method}
\alias{OAsymTotalVarDist,AbscontDistribution,DiscreteDistribution-method}
\alias{OAsymTotalVarDist,DiscreteDistribution,DiscreteDistribution-method}
\alias{OAsymTotalVarDist,DiscreteDistribution,AbscontDistribution-method}
\alias{OAsymTotalVarDist,LatticeDistribution,DiscreteDistribution-method}
\alias{OAsymTotalVarDist,DiscreteDistribution,LatticeDistribution-method}
\alias{OAsymTotalVarDist,LatticeDistribution,LatticeDistribution-method}
\alias{OAsymTotalVarDist,numeric,DiscreteDistribution-method}
\alias{OAsymTotalVarDist,DiscreteDistribution,numeric-method}
\alias{OAsymTotalVarDist,numeric,AbscontDistribution-method}
\alias{OAsymTotalVarDist,AbscontDistribution,numeric-method}
\alias{OAsymTotalVarDist,AcDcLcDistribution,AcDcLcDistribution-method}

\title{Generic function for the computation of (minimal) asymmetric total variation distance of two distributions}
\description{
  Generic function for the computation of (minimal) asymmetric total variation distance 
  \eqn{d_v^\ast}{d_v'}
  of two distributions \eqn{P} and \eqn{Q} where the distributions may be 
  defined for an arbitrary sample space \eqn{(\Omega,{\cal A})}{(Omega, A)}.
  This distance is defined as
  \deqn{d_v^\ast(P,Q)=\min_c \int |dQ-c\,dP|}{d_v(rho)(P,Q)=\min_c \int |(dQ-c dP|}
}
\usage{
OAsymTotalVarDist(e1, e2, ...)
\S4method{OAsymTotalVarDist}{AbscontDistribution,AbscontDistribution}(e1,e2, 
             rel.tol = .Machine$double.eps^0.3,  Ngrid = 10000,
             TruncQuantile = getdistrOption("TruncQuantile"),
             IQR.fac = 15)
\S4method{OAsymTotalVarDist}{AbscontDistribution,DiscreteDistribution}(e1,e2, ...)
\S4method{OAsymTotalVarDist}{DiscreteDistribution,AbscontDistribution}(e1,e2,  ...)
\S4method{OAsymTotalVarDist}{DiscreteDistribution,DiscreteDistribution}(e1,e2, ...)
\S4method{OAsymTotalVarDist}{numeric,DiscreteDistribution}(e1, e2,  ...)
\S4method{OAsymTotalVarDist}{DiscreteDistribution,numeric}(e1, e2,  ...)
\S4method{OAsymTotalVarDist}{numeric,AbscontDistribution}(e1, e2,  asis.smooth.discretize = "discretize", 
            n.discr = getdistrExOption("nDiscretize"), low.discr = getLow(e2),
            up.discr = getUp(e2), h.smooth = getdistrExOption("hSmooth"),
             rel.tol = .Machine$double.eps^0.3, Ngrid = 10000,
             TruncQuantile = getdistrOption("TruncQuantile"),
             IQR.fac = 15)
\S4method{OAsymTotalVarDist}{AbscontDistribution,numeric}(e1, e2, 
            asis.smooth.discretize = "discretize", 
            n.discr = getdistrExOption("nDiscretize"), low.discr = getLow(e1),
            up.discr = getUp(e1), h.smooth = getdistrExOption("hSmooth"),
             rel.tol = .Machine$double.eps^0.3, Ngrid = 10000,
             TruncQuantile = getdistrOption("TruncQuantile"),
             IQR.fac = 15)
\S4method{OAsymTotalVarDist}{AcDcLcDistribution,AcDcLcDistribution}(e1, e2,
             rel.tol = .Machine$double.eps^0.3, Ngrid = 10000,
             TruncQuantile = getdistrOption("TruncQuantile"),
             IQR.fac = 15)
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
  \item{rel.tol}{relative tolerance for \code{distrExIntegrate} and \code{uniroot}}
  \item{Ngrid}{How many grid points are to be evaluated to determine the range
   of the likelihood ratio?},
  \item{TruncQuantile}{Quantile the quantile based integration bounds (see details)}
  \item{IQR.fac}{Factor for the scale based integration bounds (see details)}
  \item{\dots}{ further arguments to be used in particular methods (not in package \pkg{distrEx})}
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
  
  Again in the absolutely continuous case, to determine the range of the 
  likelihood ratio, we evaluate this ratio on a grid constructed as follows:
  \code{x.range <- c(seq(low, up, length=Ngrid/3),
                     q(e1)(seq(0,1,length=Ngrid/3)*.999),
                     q(e2)(seq(0,1,length=Ngrid/3)*.999))}  
  
  Finally, for both discrete and absolutely continuous case,
  we clip this ratio downwards by \code{1e-10} and upwards by \code{1e10}
  
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
\value{ OAsymmetric Total variation distance of \code{e1} and \code{e2} }
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
  \item{e1 = "AcDcLcDistribution", e2 = "AcDcLcDistribution":}{
    Total variation distance of mixed discrete and absolutely continuous
    univariate distributions.
  }
}}
\references{
  to be filled; 
  Agostinelli, C and Ruckdeschel, P. (2009): A simultaneous inlier and outlier model
  by asymmetric total variation distance.
}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{TotalVarDist-methods}}, \code{\link{ContaminationSize}}, 
    \code{\link{KolmogorovDist}}, \code{\link{HellingerDist}}, 
    \code{\link[distr]{Distribution-class}}}
\examples{
OAsymTotalVarDist(Norm(), Gumbel())
OAsymTotalVarDist(Norm(), Td(10))
OAsymTotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
OAsymTotalVarDist(Pois(10), Binom(size = 20)) 

x <- rnorm(100)
OAsymTotalVarDist(Norm(), x)
OAsymTotalVarDist(x, Norm(), asis.smooth.discretize = "smooth")

y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
OAsymTotalVarDist(y, Norm())
OAsymTotalVarDist(y, Norm(), asis.smooth.discretize = "smooth")

OAsymTotalVarDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))
}
\concept{distance}
\keyword{distribution}
