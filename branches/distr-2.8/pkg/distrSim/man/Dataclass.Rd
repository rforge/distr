\name{Dataclass}
\alias{Dataclass}
\alias{Dataclass-method}
\alias{Dataclass,DataframeorSeqDataFrames-method}
\alias{Dataclass,ArrayorNULLorVector-method}
\alias{Dataclass,array-method}
\alias{Dataclass,matrix-method}
\title{Generating function for "Dataclass" }
\description{
  Generates an object of class "Dataclass".
}
\usage{Dataclass(Data, ...)
\S4method{Dataclass}{ArrayorNULLorVector}(Data, filename = NULL, name = "Data-Set")
\S4method{Dataclass}{array}(Data, filename = NULL, name = "Data-Set")
\S4method{Dataclass}{matrix}(Data, filename = NULL, name = "Data-Set")
\S4method{Dataclass}{DataframeorSeqDataFrames}(Data, filename = NULL, name = "Data-Set")
}
\arguments{
  \item{Data}{the data to be filled in}
  \item{filename}{ Object of class \code{"character"}: the filename the data shall be saved}
  \item{name}{       Object of class \code{"character"}: a name for the Data}
  \item{\dots}{ additional parameters. }
}


\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de},\cr
  }

\seealso{
\code{\link{Dataclass-class}}
}
%\examples{}
\keyword{manip}
\concept{S4 data class}
\concept{S4 simulation class}
