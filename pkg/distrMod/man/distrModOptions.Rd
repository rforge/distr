\name{distrModOptions}
\alias{distrModOptions}
\alias{getdistrModOption}
\alias{nDiscretize}
\alias{hSmooth}

\title{Function to change the global variables of the package `distrMod' }
\description{With \code{distrModOptions} you can inspect and change 
the global variables of the package \pkg{distrMod}. }
\usage{
distrModOptions(...)
getdistrModOption(x)
}
\arguments{
  \item{\dots}{ any options can be defined, using name = value or by passing a list of such tagged values. }
  \item{x}{ a character string holding an option name.}
}
%\details{}
\value{
  \code{distrModOptions()} returns a list of the global variables.\cr
  \code{distrModOptions(x)} returns the global variable \var{x}.\cr
  \code{getdistrModOption(x)} returns the global variable \var{x}.\cr
  \code{distrModOptions(x=y)} sets the value of the global variable \var{x} to \var{y}.
}
\section{Global Options}{
\describe{
  \item{nDiscretize:}{ number of support values used for the discretization
    of objects of class \code{"AbscontDistribution"}. }
  \item{hSmooth:}{ smoothing parameter to smooth objects of class 
  \code{"DiscreteDistribution"}. This is done via convolution with the
    normal distribution \code{Norm(mean = 0, sd = hSmooth)}.}
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{}
\seealso{\code{\link[base]{options}}, \code{\link[base]{getOption}}}
\examples{
distrModOptions()
distrModOptions("nDiscretize")
distrModOptions("nDiscretize" = 1000)
# or
distrModOptions(nDiscretize = 1000)
getdistrModOption("hSmooth")
}
\keyword{misc}
\keyword{distribution}
\concept{global options}
\concept{options}
