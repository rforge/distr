\name{Estimate-class}
\docType{class}
\alias{Estimate-class}
\alias{name,Estimate-method}
\alias{name<-,Estimate-method}
\alias{estimate}
\alias{estimate,Estimate-method}
\alias{estimate.call}
\alias{estimate.call,Estimate-method}
\alias{Infos}
\alias{Infos,Estimate-method}
\alias{samplesize}
\alias{samplesize,Estimate-method}
\alias{asvar}
\alias{asvar,Estimate-method}
\alias{asvar<-}
\alias{asvar<-,Estimate-method}
\alias{nuisance,Estimate-method}
\alias{main,Estimate-method}
\alias{Infos<-}
\alias{Infos<-,Estimate-method}
\alias{addInfo<-}
\alias{addInfo<-,Estimate-method}
\alias{show,Estimate-method}
\alias{print,Estimate-method}

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
    \item{\code{estimate.call}:}{Object of class \code{"call"}:
      call by which estimate was produced.}
    \item{\code{Infos}:}{ object of class \code{"matrix"}
      with two columns named \code{method} and \code{message}:
      additional informations. }
    \item{\code{asvar}:}{ object of class \code{"OptionalMatrix"}
      which may contain the asymptotic (co)variance of the estimator. }
    \item{\code{samplesize}:}{ object of class \code{"numeric"} ---
      the samplesize at which the estimate was evaluated. }
    \item{\code{nuis.idx}:}{ object of class \code{"OptionalNumeric"}: 
     indices of \code{estimate} belonging to the nuisance part}
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

    \item{estimate.call}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{estimate.call}. }

    \item{samplesize}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{samplesize}. }

    \item{asvar}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{asvar}. }
    \item{asvar<-}{\code{signature(object = "Estimate")}: 
      replacement function for slot \code{asvar}. }

    \item{nuisance}{\code{signature(object = "Estimate")}: 
      accessor function for \code{nuisance} part of slot \code{estimate}. }
    \item{main}{\code{signature(object = "Estimate")}: 
      accessor function for \code{main} part of slot \code{estimate}. }

    \item{Infos}{\code{signature(object = "Estimate")}: 
      accessor function for slot \code{Infos}. }

    \item{Infos<-}{\code{signature(object = "Estimate")}: 
      replacement function for slot \code{Infos}. }

    \item{addInfo<-}{\code{signature(object = "Estimate")}: 
      function to add an information to slot \code{Infos}. }

    \item{show}{\code{signature(object = "Estimate")}}
    \item{print}{\code{signature(object = "Estimate")}: just as \code{show}, 
     but with additional arguments \code{digits}.}
  }
}
\note{Parts of the code for methods \code{show} and \code{print} has been
borrowed from \code{print.fitdistr} in package \pkg{MASS} by B.D. Ripley.}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{Estimator}}}
\examples{
x <- rnorm(100)
Estimator(x, estimator = mean, name = "mean")
}
\concept{estimate}
\keyword{classes}
