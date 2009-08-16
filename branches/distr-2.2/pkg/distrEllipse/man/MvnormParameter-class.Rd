\name{MVNormParameter-class}
\docType{class}
\alias{MVNormParameter-class}
\alias{mean}
\alias{mean,MVNormParameter-method}
\alias{sigma}
\alias{sigma,MVNormParameter-method}

\title{Paramter of a multivariate normal  distribution}
\description{The class of the parameter of MVNorm distributions.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MVNormParameter", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{loc}:}{ numeric; center / location of the distribution. }
    \item{\code{scale}:}{ matrix; the scale matrix; the number of rows of this 
    matrix must be the same as the length of \code{location}.}
    \item{\code{name}:}{ default name is 
      \dQuote{parameter of a Elliptical distribution}. }
  }
}
\section{Extends}{
Class \code{"EllipticalParameter"}, directly.\cr
Class \code{"Parameter"}, by class \code{"EllipticalParameter"}.\cr
Class \code{"OptionalParameter"}, by class \code{"Parameter"}.
}
\section{Methods}{
  \describe{
    \item{mean}{\code{signature(object = "MVNormParameter")}: access method for
      slot \code{location}. }
    \item{sigma}{\code{signature(x = "MVNormParameter")}: utility function; returns
      \code{S\%*\%t(S)
      } for \code{S=scale(x)}. }
  }
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{MVNormDistribution-class}}, \code{\link[distr]{Parameter-class}}}
\examples{
new("MVNormParameter")
}
\concept{MVNorm distribution}
\keyword{distribution}
\concept{parameter}
\concept{S4 parameter class}
\keyword{models}
\concept{generating function}
