\name{Subsetting-methods}
\docType{methods}
\alias{[-methods}
\alias{[<--methods}
\alias{[,SeqDataFrames-method}
\alias{[<-,SeqDataFrames-method}
\title{ Subsetting/Indexing methods for SeqDataFrames objects in Package `distrSim'}
\description{Operators acting on \code{SeqDataFrames} objects to extract
     or replace parts.}
\usage{
## "["(x,i,j,k, drop = FALSE) # is: x[i,j,k, drop = FALSE]
\S4method{[}{SeqDataFrames}(x, i, j, k, drop = FALSE)

## "[<-"(x,i,j,k, value)      # is: x[i,j,k] <- value
\S4method{[}{SeqDataFrames}(x, i, j, k) <- value
}
\arguments{
  \item{x}{ object of class \code{SeqDataFrames} from which to extract element(s) or in which to
          replace element(s).} 
  \item{i}{ observation index; may be missing}
  \item{j}{ observation dimension index; may be missing}
  \item{k}{ run index; may be missing}
  \item{drop}{ as in the general indexing functions}
  \item{value}{ modication to be assigned to}
}
\section{Methods}{
  \describe{
    \item{"["}{\code{signature(x = "SeqDataFrames")}:
      access method for indices for objects of class \code{SeqDataFrames}}
    \item{"[<-"}{\code{signature(x = "SeqDataFrames")}:
      replacement method for indices for objects of class \code{SeqDataFrames}}
  }
}

\value{again an object of class \code{SeqDataFrames} with the prescribed indices / values}
\seealso{\code{\link[base]{"["}}}
\keyword{methods}
\examples{
s0 <- matrix(1:6,3,2)
d0 <- data.frame(s0)
d1 <- data.frame(s0 + 3)
SF <- seqDataFrames(d0, d1)
SF[1,2,1]

}
\concept{subsetting}
\concept{indexing}
\concept{accessor function}
\concept{replacement function}
