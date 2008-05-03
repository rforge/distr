\name{Dataclass}
\alias{Dataclass}
\alias{Dataclass-method}
\alias{Dataclass,DataframeorSeqDataFrames-method}
\alias{Dataclass,ArrayorNULLorVector-method}
\title{Generating function for "Dataclass" }
\description{
  Generates an object of class "Dataclass".
}
\usage{Dataclass(Data, ...)
\S4method{Dataclass}{ArrayorNULLorVector}(Data, filename = NULL, name = "Data-Set")
\S4method{Dataclass}{DataframeorSeqDataFrames}(Data, filename = NULL, name = "Data-Set",
          Obsdimnames = NULL, Runnames = NULL)
}
\arguments{
  \item{Data}{the data to be filled in}
  \item{\dots}{ additional parameters. }
  \item{filename}{ Object of class \code{"character"}: the filename the data shall be saved}
  \item{name}{       Object of class \code{"character"}: a name for the Data}
  \item{Obsdimnames}{optional names for the observation dimensions}
  \item{Runnames}{optional names for the runs}
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@web.de},\cr
  }

\seealso{
\code{\link{Dataclass-class}}
}
%\examples{}
\keyword{manip}
\concept{S4 data class}
\concept{S4 simulation class}
