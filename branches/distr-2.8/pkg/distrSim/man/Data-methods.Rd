\name{Data-methods}
\docType{methods}
\alias{Data-methods}
\alias{Data<--methods}
\alias{Data}
\alias{Data<-}
\alias{Data,Dataclass-method}
\alias{Data<-,Dataclass-method}
\alias{Data<-,Simulation-method}
\alias{Data<-,Contsimulation-method}
\title{ Methods for Function Data in Package `distrSim' }
\description{ Methods to access and change the \code{Data}-slot }
\section{Methods}{\describe{
\item{Data}{\code{signature(object = "Dataclass")}: returns the data }
\item{Data<-}{\code{signature(object = "Dataclass")}: changes the data (does not work with a simulation or a contsimulation
object) }
\item{Data<-}{\code{signature(object = "Simulation")}: ERROR: A change of the data is not allowed. }
\item{Data<-}{\code{signature(object = "Contsimulation")}: ERROR: A change of the data is not allowed. }
}}
\concept{simulation}
\concept{S4 simulation class}
\concept{accessor function}
\concept{contaminated data}
\concept{realistic data}
\concept{contamination}
\keyword{methods}
\keyword{distribution}
