\name{vectororNULL-class}
\docType{class}
\alias{vectororNULL-class}
\alias{ArrayorNULLorVectororDataframeorSeqDataFrames-class}
\alias{DataframeorSeqDataFrames-class}
\alias{ArrayorNULLorVector-class}
\alias{MatrixorNULLorVector-class}

\title{Classes "vectororNULL",  "MatrixorNULLorVector", "ArrayorNULLorVector", "DataframeorSeqDataFrame" and 
      "ArrayorNULLorVectororDataframeorSeqDataFrames"}
\description{ auxiliary classes; may contain either a vector or NULL, [or a matrix, or an array, respectively], 
             cf. J. Chambers, "green book".}
\section{Objects from the Classes}{these classes are all virtual: No objects may be created from it.}
\section{Methods}{
No methods defined with classs "vectororNULL",  "MatrixorNULLorVector",  and 
"ArrayorNULLorVectororDataframeorSeqDataFrames" in the signature.
However, the generating function \code{\link{Dataclass}} dispatches according to 
"DataframeorSeqDataFrames" or "ArrayorNULLorVector".
}
\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }

\note{Dataclass-class can save data either of type "NULL" (means no data) or "vector" or "array" or "data.frame"}
\seealso{\code{\link{Dataclass-class}}}
\keyword{classes}
\concept{class union}
\concept{virtual}

