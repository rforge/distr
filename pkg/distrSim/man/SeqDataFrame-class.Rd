\name{SeqDataFrames-class}
\docType{class}
\alias{SeqDataFrames-class}
\alias{SeqDataFrames}
\alias{seqDataFrames}
\alias{obsdimnames}
\alias{obsdimnames-method}
\alias{obsdimnames,SeqDataFrames-method}
\alias{obsdimnames<-,SeqDataFrames-method}
\alias{names,SeqDataFrames-method}
\alias{names<-,SeqDataFrames-method}
\alias{runnames}
\alias{runnames-method}
\alias{runnames,SeqDataFrames-method}
\alias{runnames<-,SeqDataFrames-method}
\alias{print,SeqDataFrames-method}
\alias{show,SeqDataFrames-method}
\alias{rbind}
\alias{rbind-method}
\alias{rbind,ANY-method}
\alias{rbind,SeqDataFrames-method}
\title{Class "SeqDataFrames" }
\description{
  An object of type "SeqDataFrames" is a list of data frames, all of which
  with the same numbers and names of columns (ideally with the same data-types
  for the columns), but with possibly varying number of rows;
  with correponding index operators it behaves like a three-dimensional array
  with dimensions sample size x observation dimension x runs.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form 
  \code{SeqDataFrames(...)}, where the \dots are a list of dataframes
  with according column structure. 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{
      a list of data frames}
  }
}
\details{There is a validity method checking for each member of the list being
a data frame and for the accordance of the
column structures of the data frames.}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SeqDataFrames")}:
      returns (slices of) the data}
    \item{[<-}{\code{signature(x = "SeqDataFrames")}:
      modifies (slices of) the data}
    \item{print}{\code{signature(x = "SeqDataFrames", obs0 = NULL,
                   dims0 = NULL, runs0 = NULL,  short = FALSE, ...)}:
                 slices can be printed and, if argument \code{short== TRUE}
                 only a bounded number of dimensions is shown.   
                   }
    \item{show}{\code{signature(object = "SeqDataFrames")}: a call to
                \code{print(x)}}
    \item{names}{\code{signature(x = "SeqDataFrames")}:
      returns the names of the runs}
    \item{runnames}{\code{signature(x = "SeqDataFrames")}:
      returns the names of the runs}
    \item{obsdimnames}{\code{signature(x = "SeqDataFrames")}:
          returns the names of the observation dimensions}
    \item{obsDim}{\code{signature(x = "SeqDataFrames")}:
      returns the dimension of the observations}
    \item{runs}{\code{signature(x = "SeqDataFrames")}:
      returns the number of runs}
    \item{samplesize}{\code{signature(x = "SeqDataFrames")}:
      returns the size of the samples for each run}
    \item{rbind}{\code{signature(x = "SeqDataFrames")}:
      concatenates different a list of \code{SeqDataFrames} object (with 
      the same column structure) to a new object of class \code{SeqDataFrames}
      to do so we mask the \code{rbind} method from package \pkg{base}}
  }
}

\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@web.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }

\seealso{
\code{\link{[-methods}}
\code{\link{print-methods}}
\code{\link{summary-methods}}
}
%\examples{}
\keyword{manip}
\concept{S4 data class}
\concept{S4 simulation class}
