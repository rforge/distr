\name{Dataclass-class}
\docType{class}
\alias{Dataclass-class}
\alias{initialize,Dataclass-method}
\title{Class "Dataclass" }
\description{
  In an object of type "Dataclass" data can be saved containing any
  number of runs in any dimension. All information about the data is
  stored in a unified way.
}
\section{Objects from the Class}{
  Objects can be created by calls to the generating function, method 
  \code{Dataclass(Data, filename = "Data-set", name = "Data-Set")}. 
  A Dataclass-object includes, aside from the actual data, a filename
  and the size of the sample, the observation dimension, and the number of runs, which give the
  number of rows and columns (and, if more than one run, slices) 
  of the data array.
}
\section{Slots}{
  \describe{
    \item{\code{filename}:}{
      Object of class \code{"character"}: the filename the data shall be saved}
    \item{\code{name}:}{
      Object of class \code{"character"}: a name for the Data}
    \item{\code{Data}:}{Object of class \code{"ArrayorNULLorVectororDataframeorSeqDataFrames"}:
      the actual data, either of type "NULL" (means no data) or "vector" or "array" 
      or "Dataframe" or "SeqDataFrames"}
    \item{\code{obsDim}:}{Object of class \code{"numeric"}:
      the observation dimension of the data}
    \item{\code{runs}:}{Object of class \code{"numeric"}:
      the number of runs of the data}
    \item{\code{samplesize}:}{Object of class \code{"numeric"}:
      the size of the sample}
    \item{\code{version}:}{Object of class \code{"character"}:
      the package version under which this object was generated}
  }
}
\section{Methods}{
  \describe{
    \item{Data}{\code{signature(object = "Dataclass")}:
      returns the actual data}
    \item{Data<-}{\code{signature(object = "Dataclass")}:
      changes the data}
    \item{evaluate}{\code{signature(object = "Dataclass", estimator =
    "function")}:

      creates an object of type "Evaluation", see there for further information }
    \item{filename}{\code{signature(object = "Dataclass")}:
      returns the the filename}
    \item{filename<-}{\code{signature(object = "Dataclass")}:
      changes the the filename}
    \item{name}{\code{signature(object = "Dataclass")}:
      returns the the name}
    \item{name<-}{\code{signature(object = "Dataclass")}:
      changes the the name}
    \item{initialize}{\code{signature(.Object = "Dataclass")}:
      initialize method}
    \item{obsDim}{\code{signature(object = "Dataclass")}:
      returns the dimension of the observations}
    \item{runs}{\code{signature(object = "Dataclass")}:
      returns the number of runs}
    \item{samplesize}{\code{signature(object = "Dataclass")}:
      returns the size of the sample}
    \item{getVersion}{\code{signature(object = "Dataclass")}:
      returns the version slot of this object}
    \item{savedata}{\code{signature(object = "Dataclass")}:
      saves the object in the directory of R and also a copy without data}
    \item{plot}{\code{signature(x = "Dataclass")}:
      produces a plot of the data matrix; ; for details confer \code{\link{plot-methods}}}
    \item{print}{\code{signature(x = "Dataclass")}:
      returns filename, the observation dimension, the number of runs 
      and the size of the sample, and, if from a version > 1.8, also the package version under 
      which the object was generated}
    \item{summary}{\code{signature(object = "Dataclass")}:
      returns the same information as print, moreover a statistical
      summary for each run}
  }
}

\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }

\note{
  The saved Dataclass can be loaded with the usual load-command, the
  saved comment with the function cload.
}
\seealso{
\code{\link{Simulation-class}}
\code{\link{Contsimulation-class}}
\code{\link[distrTEst]{Evaluation-class}}
\code{\link{plot-methods}} \cr
\code{\link{print-methods}}
\code{\link{summary-methods}}
\code{\link{load}}
\code{\link{cload}}
\code{\link{savedata-methods}}
\code{\link{getVersion-methods}}
}
\examples{
D66 <- Dataclass(filename="N74", Data = matrix(1:36,6))
D66
#
D <- Dataclass(Data = array(c(1,2,3,4,5,6),c(samplesize=2,obsdim=3,Runs=1)),
               filename = "xyz.sav")
# A new object of type "Dataclass" is created.
#
isOldVersion(D) ##NO!
#
savedata(D)
# creates a file called "xyz.sav" where the information is saved and a
# copy "xyz.sav.comment" without data
Data(D) <- array(c(11,12,13,14,15,16),c(samplesize=2,obsdim=3,Runs=1)) # changes the data of D
cload("xyz.sav") # loads the object without data - it is called "D.comment"
D.comment
load("xyz.sav") # loads the original object "D"
Data(D) # the original data: matrix(c(1,2,3,4,5,6),2)
#if you have distrTEst available:
#evaluate(object = D, estimator = mean) # returns the mean of each variable
}
\keyword{manip}
\concept{S4 data class}
\concept{S4 simulation class}
