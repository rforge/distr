\name{EllipticalParameter-class}
\docType{class}
\alias{EllipticalParameter-class}
\alias{location}
\alias{location,EllipticalParameter-method}
\alias{location<-}
\alias{location<-,EllipticalParameter-method}
\alias{scale}
\alias{scale,EllipticalParameter-method}
\alias{scale<-}
\alias{scale<-,EllipticalParameter-method}

\title{Paramter of an Elliptical distributions}
\description{The class of the parameter of Elliptical distributions.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EllipticalParameter", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{loc}}{ numeric; center / location of the distribution. }
    \item{\code{scale}}{ matrix; the scale matrix; the number of rows of this 
    matrix must be the same as the length of \code{location}.}
    \item{\code{name}}{ default name is 
      \dQuote{parameter of a Elliptical distribution}. }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.\cr
Class \code{"OptionalParameter"}, by class \code{"Parameter"}.
}
\section{Methods}{
  \describe{
    \item{location}{\code{signature(object = "EllipticalParameter")}: access method for
      slot \code{location}. }
    \item{scale}{\code{signature(x = "EllipticalParameter")}: access method for
      slot \code{scale}. }
    \item{location<-}{\code{signature(object = "EllipticalParameter")}: replace method for
      slot \code{location}. }
    \item{scale<-}{\code{signature(object = "EllipticalParameter")}: replace method for
      slot \code{scale}. }
  }
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{EllipticalDistribution-class}}, \code{\link[distr]{Parameter-class}}}
\examples{
new("EllipticalParameter")
}
\concept{Elliptical distribution}
\keyword{distribution}
\concept{parameter}
\concept{S4 parameter class}
\keyword{models}
\concept{generating function}
