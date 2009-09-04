\name{MVtParameter-class}
\docType{class}
\alias{MVtParameter-class}
\alias{sigma,MVtParameter-method}
\alias{df,MVtParameter-method}
\alias{ncp,MVtParameter-method}

\title{Paramter of a multivariate t distribution}
\description{The class of the parameter of MVt distributions.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MVtParameter", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{loc}:}{ numeric; center / location of the distribution. }
    \item{\code{scale}:}{ matrix; the scale matrix; the number of rows of this 
    matrix must be the same as the length of \code{location}.}
    \item{\code{df}:}{ integer; the degrees of freedom. }
    \item{\code{ncp}:}{ positive real; the non-centrality parameter.}
    \item{\code{name}:}{ default name is 
      \dQuote{parameter of a Elliptical distribution}. }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.\cr
Class \code{"OptionalParameter"}, by class \code{"Parameter"}.
}
\section{Methods}{
  \describe{
    \item{mean}{\code{signature(object = "MVnormParameter")}: access method for
      slot \code{location}. }
    \item{sigma}{\code{signature(x = "MVnormParameter")}: utility function; returns
      \code{S\%*\%t(S)
      } for \code{S=scale(x)}. }
    \item{ncp}{\code{signature(object = "MVnormParameter")}: access method for
      slot \code{ncp}. }
    \item{df}{\code{signature(x = "MVnormParameter")}: access method for slot
    \code{df}. }
  }
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{MVtDistribution-class}}, \code{\link[distr]{Parameter-class}}}
\examples{
new("MVtParameter")
}
\concept{MVt distribution}
\keyword{distribution}
\concept{parameter}
\concept{S4 parameter class}
\keyword{models}
\concept{generating function}
