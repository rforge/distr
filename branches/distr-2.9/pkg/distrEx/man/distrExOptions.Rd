\name{distrExOptions}
\alias{distrExOptions}
\alias{distrExoptions}
\alias{getdistrExOption}
\alias{MCIterations}
\alias{GLIntegrateTruncQuantile}
\alias{GLIntegrateOrder}
\alias{ElowerTruncQuantile}
\alias{EupperTruncQuantile}
\alias{ErelativeTolerance}
\alias{m1dfLowerTruncQuantile}
\alias{m1dfRelativeTolerance}
\alias{m2dfLowerTruncQuantile}
\alias{m2dfRelativeTolerance}
\alias{nDiscretize}
\alias{hSmooth}
\alias{IQR.fac}
\alias{propagate.names.functionals}

\title{Function to change the global variables of the package `distrEx' }
\description{With \code{distrExOptions} you can inspect and change 
  the global variables of the package \pkg{distrEx}. }
\usage{
distrExOptions(...)
distrExoptions(...)
getdistrExOption(x)
}
\arguments{
  \item{\dots}{ any options can be defined, using name = value or by passing a list of such tagged values. }
  \item{x}{ a character string holding an option name.}
}
%\details{}
\value{
  \code{distrExOptions()} returns a list of the global variables.\cr
  \code{distrExOptions(x)} returns the global variable \var{x}.\cr
  \code{getdistrExOption(x)} returns the global variable \var{x}.\cr
  \code{distrExOptions(x=y)} sets the value of the global variable \var{x} to \var{y}.
}
\section{distrExoptions}{
For compatibility with spelling in package \pkg{distr}, \code{distrExoptions} is 
just a synonym to \code{distrExOptions}.
}
\section{Global Options}{
\describe{
  \item{MCIterations:}{ number of Monte-Carlo iterations used for crude 
    Monte-Carlo integration; defaults to \code{1e5}. }
  \item{GLIntegrateTruncQuantile:}{ If \code{integrate} fails and there are 
    infinite integration limits, the function \code{GLIntegrate} is 
    called inside of \code{distrExIntegrate} with the corresponding quantiles 
    \code{GLIntegrateTruncQuantile} respectively,  
    1 - \code{GLIntegrateTruncQuantile} as finite integration limits; defaults
    to \code{10*.Machine$double.eps}. }
  \item{GLIntegrateOrder:}{ The order used for the Gauss-Legendre integration
    inside of \code{distrExIntegrate}; defaults to 500. }
  \item{ElowerTruncQuantile:}{ The lower limit of integration used inside of 
    \code{E} which corresponds to the \code{ElowerTruncQuantile}-quantile; defaults to
    \code{1e-7}. }
  \item{EupperTruncQuantile:}{ The upper limit of integration used inside of 
    \code{E} which corresponds to the (1-\code{ElowerTruncQuantile})-quantile; defaults to
    \code{1e-7}. }
  \item{ErelativeTolerance:}{ The relative tolerance used inside of \code{E}
    when calling \code{distrExIntegrate}; defaults to \code{.Machine$double.eps^0.25}. }
  \item{m1dfLowerTruncQuantile:}{ The lower limit of integration used inside 
    of \code{m1df} which corresponds to the \code{m1dfLowerTruncQuantile}-quantile; defaults to 0. }
  \item{m1dfRelativeTolerance:}{ The relative tolerance used inside of \code{m1df}
    when calling \code{distrExIntegrate}; defaults to \code{.Machine$double.eps^0.25}. }
  \item{m2dfLowerTruncQuantile:}{ The lower limit of integration used inside 
    of \code{m2df} which corresponds to the \code{m2dfLowerTruncQuantile}-quantile;
    defaults to 0. }
  \item{m2dfRelativeTolerance:}{ The relative tolerance used inside of \code{m2df}
    when calling \code{distrExIntegrate}; defaults to \code{.Machine$double.eps^0.25}. }
  \item{nDiscretize:}{ number of support values used for the discretization
    of objects of class \code{"AbscontDistribution"}; defaults to 100. }
  \item{hSmooth:}{ smoothing parameter to smooth objects of class 
    \code{"DiscreteDistribution"}. This is done via convolution with the
    normal distribution \code{Norm(mean = 0, sd = hSmooth)}; defaults to 0.05.}
  \item{IQR.fac:}{for determining sensible integration ranges, we use
                  both quantile and scale based methods; for the scale based
                  method we use the median of the distribution \eqn{\pm}{+-}
                  \code{IQR.fac}\eqn{\times}{*} the IQR; defaults to 15.}
  \item{propagate.names.functionals}{should names obtained from parameter
     coordinates be propagated to return values of specific S4 methods
     for functionals; defaults to \code{TRUE}. }
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[base]{options}}, \code{\link[base:options]{getOption}}}
\examples{
distrExOptions()
distrExOptions("ElowerTruncQuantile")
distrExOptions("ElowerTruncQuantile" = 1e-6)
# or
distrExOptions(ElowerTruncQuantile = 1e-6)
getdistrExOption("ElowerTruncQuantile")
}
\keyword{misc}
\keyword{distribution}
\concept{global options}
\concept{options}
