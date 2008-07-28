\name{MCEstimate-class}
\docType{class}
\alias{MCEstimate-class}
\alias{criterion}
\alias{criterion,MCEstimate-method}
\alias{criterion<-}
\alias{criterion<-,MCEstimate-method}

\title{MCEstimate-class.}
\description{Class of minimum criterion estimates.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("MCEstimate", ...)}.
  More frequently they are created via the generating functions
  \code{MCEstimator}, \code{MDEstimator} or \code{MLEstimator}. 
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}:
      name of the estimator. }
    \item{\code{estimate}:}{Object of class \code{"ANY"}:
      estimate.}
    \item{\code{criterion}:}{Object of class \code{"numeric"}:
      minimum value of the considered criterion.}
    \item{\code{Infos}:}{ object of class \code{"matrix"}
      with two columns named \code{method} and \code{message}:
      additional informations. }
  }
}
\section{Extends}{
Class \code{"Estimate"}, directly.
}
\section{Methods}{
  \describe{
    \item{criterion}{\code{signature(object = "MCEstimate")}: 
      accessor function for slot \code{criterion}. }

    \item{criterion<-}{\code{signature(object = "MCEstimate")}: 
      replacement function for slot \code{criterion}. }
  }
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{Estimate-class}}, \code{\link{MCEstimator}}, 
         \code{\link{MDEstimator}}, \code{\link{MLEstimator}}}
\examples{
## (empirical) Data
x <- rgamma(50, scale = 0.5, shape = 3)

## parametric family of probability measures
G <- GammaFamily(scale = 1, shape = 2)

MDEstimator(x, G)
MLEstimator(x, G)
}
\concept{estimate}
\keyword{classes}
