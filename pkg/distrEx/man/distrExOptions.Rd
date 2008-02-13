\name{distrExOptions}
\alias{distrExOptions}
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

\title{Function to change the global variables of the package `distrEx' }
\description{With \code{distrExOptions} you can inspect and change 
  the global variables of the package \pkg{distrEx}. }
\usage{
distrExOptions(...)
getdistrExOption(x)
}
\arguments{
  \item{\dots}{ any options can be defined, using name = value or by passing a list of such tagged values. }
  \item{x}{ a character string holding an option name.}
}
%\details{}
\value{
  \code{distrExOptions()} returns a list of the global variables.\newline
  \code{distrExOptions(x)} returns the global variable \var{x}.\newline
  \code{getdistrExOption(x)} returns the global variable \var{x}.\newline
  \code{distrExOptions(x=y)} sets the value of the global variable \var{x} to \var{y}.
}
\section{Global Options}{
\describe{
  \item{MCIterations:}{ number of Monte-Carlo iterations used for crude 
    Monte-Carlo integration. }
  \item{GLIntegrateTruncQuantile:}{ If \code{integrate} fails and there are 
    infinite integration limits, the function \code{GLIntegrate} is 
    called inside of \code{distrExIntegrate} with the corresponding quantiles 
    \code{GLIntegrateTruncQuantile} respectively,  
    1 - \code{GLIntegrateTruncQuantile} as finite integration limits. }
  \item{GLIntegrateOrder:}{ The order used for the Gauss-Legendre integration
    inside of \code{distrExIntegrate}. }
  \item{ElowerTruncQuantile:}{ The lower limit of integration used inside of 
    \code{E} which corresponds to the \code{ElowerTruncQuantile}-quantile. }
  \item{EupperTruncQuantile:}{ The upper limit of integration used inside of 
    \code{E} which corresponds to the (1-\code{ElowerTruncQuantile})-quantile. }
  \item{ErelativeTolerance:}{ The relative tolerance used inside of \code{E}
    when calling \code{distrExIntegrate}. }
  \item{m1dfLowerTruncQuantile:}{ The lower limit of integration used inside 
    of \code{m1df} which corresponds to the \code{m1dfLowerTruncQuantile}-quantile. }
  \item{m1dfRelativeTolerance:}{ The relative tolerance used inside of \code{m1df}
    when calling \code{distrExIntegrate}. }
  \item{m2dfLowerTruncQuantile:}{ The lower limit of integration used inside 
    of \code{m2df} which corresponds to the \code{m2dfLowerTruncQuantile}-quantile. }
  \item{m2dfRelativeTolerance:}{ The relative tolerance used inside of \code{m2df}
    when calling \code{distrExIntegrate}. }
  \item{nDiscretize:}{ number of support values used for the discretization
    of objects of class \code{"AbscontDistribution"}. }
  \item{hSmooth:}{ smoothing parameter to smooth objects of class 
    \code{"DiscreteDistribution"}. This is done via convolution with the
    normal distribution \code{Norm(mean = 0, sd = hSmooth)}.}
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[base]{options}}, \code{\link[base]{getOption}}}
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
