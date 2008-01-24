\name{LMParameter-class}
\docType{class}
\alias{LMParameter-class}
\alias{show,LMParameter-method}

\title{Parameter of a linear regression model}
\description{Parameter of a linear regression model 
\deqn{y = \mu + x^\tau\theta + \sigma u}{y = mu + t(x)theta + sigma u}
with intercept \eqn{\mu}{mu}, regression parameter \eqn{\theta}{theta} 
and error scale \eqn{\sigma}{sigma}. }
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("LMParameter", ...)}.
  More frequently they are created via the generating function 
  \code{LMParameter}. 
}
\section{Slots}{
  \describe{
    \item{\code{theta}:}{ numeric vector: 
      regression parameter. }
    \item{\code{intercept}:}{ real number: 
      intercept parameter. }
    \item{\code{scale}:}{ positive real number: 
      scale paramter. }
    \item{\code{name}:}{ character vector:
      the default name is \dQuote{parameter of a linear regression model}. }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.\cr
Class \code{"OptionalParameter"}, by class \code{"Parameter"}.
}
\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "LMParameter")}}
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distr]{Parameter-class}}, \code{\link{LMParameter}}}
\examples{
  new("LMParameter")
}
\concept{parameter}
\keyword{distribution}
\concept{S4 parameter class}
\concept{S4 linear model class}
