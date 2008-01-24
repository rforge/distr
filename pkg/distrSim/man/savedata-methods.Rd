\name{savedata-methods}
\docType{methods}
\alias{savedata-methods}
\alias{savedata}
\alias{savedata,Dataclass-method}
\alias{savedata,Simulation-method}
\alias{savedata,Contsimulation-method}
\title{ Methods for Function savedata in Package `distrSim' }
\description{savedata-methods}

\section{Methods}{\describe{

\item{savedata}{\code{signature(object = "Dataclass")}: saves the object (with the data) in the directory of R }

\item{savedata}{\code{signature(object = "Simulation")}: saves the object without the data in the directory of R
    (After loading the data can be reproduced by using simulate.) }

\item{savedata}{\code{signature(object = "Contsimulation")}: saves the object without the data in the directory of R
    (After loading the data can be reproduced by using simulate.) }
}}

\note{For an example, see \code{\link{Simulation-class}}
and \code{\link{Contsimulation-class}}
}

\seealso{
\code{\link{Dataclass-class}}
\code{\link{Simulation-class}}
\code{\link{Contsimulation-class}}
\code{\link[distrTEst]{Evaluation-class}}
}
\keyword{methods}
\keyword{file}
\concept{save}
