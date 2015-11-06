\name{SSTdParameter-class}
\docType{class}
\alias{SSTdParameter-class}
\alias{mean,SSTdParameter-method}
\alias{sd,SSTdParameter-method}
\alias{xi,SSTdParameter-method}
\alias{nu,SSTdParameter-method}


\title{Parameter of an SSTd distributions}
\description{The class of the parameter of an SSTd distribution.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SSTdParameter", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{mean}}{ real number: location parameter of
      a SSTd distribution. }
    \item{\code{sd}}{ real number: scale parameter of
      a SSTd distribution. }
    \item{\code{xi}}{ real number: shape parameter of
      a SSTd distribution. }
    \item{\code{nu}}{ positive number: the degree of freedom parameter
     of a SSTd distribution. }
    \item{\code{name}}{ default name is
      \dQuote{parameter of a SSTd distribution}. }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.\cr
Class \code{"OptionalParameter"}, by class \code{"Parameter"}.
}
\section{Methods}{
  \describe{
    \item{mean}{\code{signature(object = "SSTdParameter")}: access method for
      slot \code{mean}. }
    \item{sd}{\code{signature(object = "SSTdParameter")}: access method for
      slot \code{sd}. }
    \item{xi}{\code{signature(object = "SSTdParameter")}: access method for
      slot \code{xi}. }
    \item{nu}{\code{signature(object = "SSTdParameter")}: access method for
      slot \code{nu}. }
  }
}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
%\note{}
\seealso{\code{\link{SSTd-class}}, \code{\link[distr]{Parameter-class}}}
\examples{
P <- new("SSTdParameter")
mean(P)
sd(P)
xi(P)
nu(P)
P
}
\concept{SSTd distribution}
\keyword{distribution}
\concept{parameter}
\concept{S4 parameter class}
\keyword{models}
\concept{generating function}
