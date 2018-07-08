\name{SNormParameter-class}
\docType{class}
\alias{SNormParameter-class}
\alias{mean,SNormParameter-method}
\alias{sd,SNormParameter-method}
\alias{xi,SNormParameter-method}


\title{Parameter of an SNorm distributions}
\description{The class of the parameter of an SNorm distribution.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SNormParameter", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{mean}}{ real number: location parameter of
      a SNorm distribution. }
    \item{\code{sd}}{ real number: scale parameter of
      a SNorm distribution. }
    \item{\code{name}}{ default name is
      \dQuote{parameter of a SNorm distribution}. }
    \item{\code{xi}}{ real number: shape parameter of
      a SNorm distribution. }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.\cr
Class \code{"OptionalParameter"}, by class \code{"Parameter"}.
}
\section{Methods}{
  \describe{
    \item{mean}{\code{signature(object = "SNormParameter")}: access method for
      slot \code{mean}. }
    \item{sd}{\code{signature(object = "SNormParameter")}: access method for
      slot \code{sd}. }
    \item{xi}{\code{signature(object = "SNormParameter")}: access method for
      slot \code{xi}. }
  }
}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
%\note{}
\seealso{\code{\link{SNorm-class}}, \code{\link[distr]{Parameter-class}}}
\examples{
P <- new("SNormParameter")
mean(P)
sd(P)
xi(P)
P
}
\concept{SNorm distribution}
\keyword{distribution}
\concept{parameter}
\concept{S4 parameter class}
\keyword{models}
\concept{generating function}
