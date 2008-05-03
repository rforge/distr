\name{cload}
\alias{cload}
\title{ cload }
\description{
loads the comment file from a saved \code{Dataclass} object
}
\usage{
cload(filename)
}
\arguments{
  \item{filename}{ the filename which was declared at the instantiaton of the Dataclass }
}
\details{ Uses function \code{load} to load the comment file from a saved \code{Dataclass} object. }
\value{ no value is returned }

\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@web.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }

\seealso{
\code{\link{Dataclass-class}}
\code{\link{load}}
\code{\link{savedata-methods}}
}

\examples{
# see Dataclass and Evaluation for examples
## The function is currently defined as
function(filename){
  eval.parent(parse(text=paste("load(\"",filename,".comment\")", sep = "")))
  }
}
\keyword{methods}
\keyword{file}
\concept{save}
\concept{comment}
