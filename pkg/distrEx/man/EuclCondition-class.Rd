\name{EuclCondition-class}
\docType{class}
\alias{EuclCondition-class}
\alias{Range}
\alias{Range,EuclCondition-method}
\alias{show,EuclCondition-method}

\title{Conditioning by an Euclidean space.}
\description{Conditioning by an Euclidean space.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("EuclCondition", ...)}.
  More frequently they are created via the generating function 
  \code{EuclCondition}. 
}
\section{Slots}{
  \describe{
    \item{\code{Range}}{Object of class \code{"EuclideanSpace"}. }
    \item{\code{name}}{Object of class \code{"character"}: name of condition. }
  }
}
\section{Extends}{
Class \code{"Condition"}, directly.
}
\section{Methods}{
\describe{
  \item{Range}{\code{signature(object = "EuclCondition")} accessor
    function for slot \code{Range}.}

  \item{show}{\code{signature(object = "EuclCondition")}}
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{Condition-class}}, \code{\link{EuclCondition}}}
\examples{
  new("EuclCondition")
}
\concept{conditioning}
\concept{condition}
\concept{S4 condition class}
\concept{conditional distribution}
\keyword{distribution}
