\name{Estimate-class}
\docType{class}
\alias{Estimate-class}
\alias{name,Estimate-method}
\alias{name<-,Estimate-method}
\alias{estimate}
\alias{estimate,Estimate-method}
\alias{Infos}
\alias{Infos,Estimate-method}
\alias{samplesize}
\alias{samplesize,Estimate-method}
\alias{asvar}
\alias{asvar,Estimate-method}
\alias{Infos<-}
\alias{Infos<-,Estimate-method}
\alias{addInfo<-}
\alias{addInfo<-,Estimate-method}
\alias{show,Estimate-method}

\title{Estimate-class.}
\description{Class of estimates.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Estimate", ...)}.
  More frequently they are created via the generating function 
  \code{Estimator}. 
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}:
      name of the estimator. }
    \item{\code{estimate}:}{Object of class \code{"ANY"}:
      estimate.}
    \item{\code{Infos}:}{ object of class \code{"matrix"}
      with two columns named \code{method} and \code{message}:
      additional informations. }
    \item{\code{asvar}:}{ object of class \code{"OptionalMatrix"}
      which may contain the asymptotic (co)variance of the estimator. }
    \item{\code{samplesize}:}{ object of class \code{"numeric"} ---
      the samplesize at which the estimate was evaluated. }
  }
}
\section{Methods}{
  \describe{
    \item{name}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{name}. }

    \item{name<-}{\code{signature(object = "Estimate")}: 
      replacement function for slot \code{name}. }

    \item{estimate}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{estimate}. }

    \item{samplesize}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{samplesize}. }

    \item{asvar}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{asvar}. }

    \item{Infos}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{Infos}. }

    \item{Infos<-}{\code{signature(object = "Estimate")}: 
      replacement function for slot \code{Infos}. }

    \item{Infos<-}{\code{signature(object = "Estimate")}: 
      replacement function for slot \code{Infos}. }

    \item{addInfo<-}{\code{signature(object = "Estimate")}: 
      function to add an information to slot \code{Infos}. }

    \item{show}{\code{signature(object = "Estimate")}}
  }
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{Estimator}}}
\examples{
x <- rnorm(100)
Estimator(x, estimator = mean, name = "mean")
}
\concept{estimate}
\keyword{classes}
