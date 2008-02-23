\name{Indexing-methods}
\docType{methods}
\alias{[}
\alias{[<-}
\alias{[-methods}
\alias{[<--methods}
\alias{[,SeqDataFrames-method}
\alias{[<-,SeqDataFrames-method}
\title{ Indexing methods for SeqDataFrames objects in Package `distrSim'}
\description{Operators acting on \code{SeqDataFrames} objects to extract
     or replace parts.}
\usage{ "["(x,i,j,k, drop = FALSE) # is: #  x[i,j, k, drop = FALSE]
        "[<-"(x,i,j,k, value)      # is: #  x[i,j, k] <- value
}
\arguments{
  \item{x}{ object of class \code{SeqDataFrame} from which to extract element(s) or in which to
          replace element(s).} 
  \item{i}{ observation index; may be missing}
  \item{j}{ observation dimension index; may be missing}
  \item{k}{ run index; may be missing}
  \item{drop} {as in the general indexing functions}
  \item{value} {modication to be assigned to}
}
\section{Methods}{
  \describe{
    \item{"["}{\code{signature(x = "SeqDataFrames")}:
      access method for indices for objects of class \code{SeqDataFrames}}
    \item{"[<-"}{\code{signature(x = "SeqDataFrames")}:
      replacement method for indices for objects of class \code{SeqDataFrames}}
  }
}

\value{again an object of class \code{SeqDataFrame} with the prescribed indices / values}
\seealso{\code{help("[",package="base")}}
\keyword{methods}
\concept{indexing}
\concept{accessor function}
\concept{replacement function}
