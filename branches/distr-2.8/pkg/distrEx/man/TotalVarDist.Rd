\name{TotalVarDist}
\alias{TotalVarDist}
\alias{TotalVarDist-methods}
\alias{TotalVarDist,AbscontDistribution,AbscontDistribution-method}
\alias{TotalVarDist,AbscontDistribution,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteMVDistribution,DiscreteMVDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,AbscontDistribution-method}
\alias{TotalVarDist,LatticeDistribution,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,LatticeDistribution-method}
\alias{TotalVarDist,LatticeDistribution,LatticeDistribution-method}
\alias{TotalVarDist,numeric,DiscreteDistribution-method}
\alias{TotalVarDist,DiscreteDistribution,numeric-method}
\alias{TotalVarDist,numeric,AbscontDistribution-method}
\alias{TotalVarDist,AbscontDistribution,numeric-method}
\alias{TotalVarDist,AcDcLcDistribution,AcDcLcDistribution-method}

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
\S4method{TotalVarDist}{AbscontDistribution,AbscontDistribution}(e1,e2, 
                        rel.tol=.Machine$double.eps^0.3, 
                        TruncQuantile = getdistrOption("TruncQuantile"), 
                        IQR.fac = 15, ..., diagnostic = FALSE)
\S4method{TotalVarDist}{AbscontDistribution,DiscreteDistribution}(e1,e2, ...)
\S4method{TotalVarDist}{DiscreteDistribution,AbscontDistribution}(e1,e2, ...)
\S4method{TotalVarDist}{DiscreteDistribution,DiscreteDistribution}(e1,e2, ...)
\S4method{TotalVarDist}{numeric,DiscreteDistribution}(e1, e2, ...)
\S4method{TotalVarDist}{DiscreteDistribution,numeric}(e1, e2, ...)
\S4method{TotalVarDist}{numeric,AbscontDistribution}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrExOption("nDiscretize"), low.discr = getLow(e2),
            up.discr = getUp(e2), h.smooth = getdistrExOption("hSmooth"),
            rel.tol = .Machine$double.eps^0.3, 
            TruncQuantile = getdistrOption("TruncQuantile"), IQR.fac = 15, ...,
            diagnostic = FALSE)
\S4method{TotalVarDist}{AbscontDistribution,numeric}(e1, e2, asis.smooth.discretize = "discretize", 
            n.discr = getdistrExOption("nDiscretize"), low.discr = getLow(e1),
            up.discr = getUp(e1), h.smooth = getdistrExOption("hSmooth"),
            rel.tol = .Machine$double.eps^0.3, 
            TruncQuantile = getdistrOption("TruncQuantile"), IQR.fac = 15, ...,
            diagnostic = FALSE)
\S4method{TotalVarDist}{AcDcLcDistribution,AcDcLcDistribution}(e1, e2,                         
                        rel.tol = .Machine$double.eps^0.3, 
                        TruncQuantile = getdistrOption("TruncQuantile"), 
                        IQR.fac = 15, ..., diagnostic = FALSE)

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

  Diagnostics on the involved integrations are available if argument
  \code{diagnostic} is \code{TRUE}. Then there is attribute \code{diagnostic}
  attached to the return value, which may be inspected
  and accessed through \code{\link[distrExIntegrate]{showDiagnostic}} and
  \code{\link[distrExIntegrate]{getDiagnostic}}.
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
  \item{e1 = "AcDcLcDistribution", e2 = "AcDcLcDistribution":}{
    Total variation distance of mixed discrete and absolutely continuous
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
\seealso{\code{\link{TotalVarDist-methods}}, \code{\link{ContaminationSize}}, 
    \code{\link{KolmogorovDist}}, \code{\link{HellingerDist}}, 
    \code{\link[distr]{Distribution-class}}}
\examples{
TotalVarDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)))
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
