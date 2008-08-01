\name{Confint-class}
\docType{class}
\alias{Confint-class}
\alias{type,Confint-method}
\alias{estimate.call,Confint-method}
\alias{confint,Confint-method}
\alias{name.estimate}
\alias{name.estimate,Confint-method}
\alias{trafo.estimate}
\alias{trafo.estimate,Confint-method}
\alias{nuisance.estimate}
\alias{nuisance.estimate,Confint-method}
\alias{show,Confint-method}
\alias{print,Confint-method}

\title{Confint-class}
\description{Return value S4 classes for method ``confint''.}
\section{Objects from the Class}{
  Objects could in principle be created by calls of the 
  form \code{new("Confint", ...)}.
  The preferred form is to have them created via a call to 
  \code{\link{confint}}.
}
\section{Slots}{
  \describe{
    \item{\code{type}:}{Object of class \code{"character"}:
      type of the confidence interval (asymptotic, bootstrap,\dots). }
    \item{\code{confint}:}{Object of class \code{"array"}:
      the confidence interval(s).}
    \item{\code{estimate.call}:}{Object of class \code{"call"}:
      the estimate(s) for which the confidence intervals are produced.}
    \item{\code{name.estimate}:}{Object of class \code{"character"}:
      the name of the estimate(s) for which the confidence intervals are produced.}
    \item{\code{trafo.estimate}:}{Object of class \code{"matrix"}:
      the trafo/derivative matrix of the estimate(s) for which 
      the confidence intervals are produced.}
    \item{\code{nuisance.estimate}:}{Object of class \code{"OptionalNumeric"}:
      the nuisance parameter (if any) at which the confidence 
      intervals are produced.}
  }
}
\section{Methods}{
  \describe{
    \item{type}{\code{signature(object = "Confint")}: 
      accessor function for slot \code{type}. }

    \item{confint}{\code{signature(object = "Confint")}: 
      accessor function for slot \code{type}. }
    
    \item{estimate.call}{\code{signature(object = "Confint")}: 
      accessor function for slot \code{estimate.call}. }

    \item{name.estimate}{\code{signature(object = "Confint")}: 
      accessor function for slot \code{name.estimate}. }

    \item{trafo.estimate}{\code{signature(object = "Confint")}: 
      accessor function for slot \code{trafo.estimate}. }

    \item{nuisance.estimate}{\code{signature(object = "Confint")}: 
      accessor function for slot \code{nuisance.estimate}. }

    \item{show}{\code{signature(object = "Confint")}: shows a detailed view
     of the object; slot \code{nuisance.estimate} is only shown if non-null,
     and slot \code{trafo.estimate} only if different from a unit matrix.}
    \item{print}{\code{signature(object = "Confint")}: just as \code{show}, 
     but with additional arguments \code{digits}.}
  }
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{Estimator}}, \code{\link{confint}}, \code{\link{Estimate-class}},
\code{\link{trafo-methods}}}
\examples{
}
\concept{confidence interval}
\keyword{classes}
