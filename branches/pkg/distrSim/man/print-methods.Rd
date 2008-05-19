\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,Dataclass-method}
\alias{print,Simulation-method}
\alias{print,Contsimulation-method}
\alias{show-methods}
\alias{show,Dataclass-method}
\alias{show,Simulation-method}
\alias{show,Contsimulation-method}
\title{ Methods for Function print/show in Package `distrSim' }
\description{print and show - methods}

\section{Methods}{\describe{

\item{print}{\code{signature(x = "SeqDataFrames")}: extra argument \code{short = FALSE}; if \code{TRUE}
 the output is cut according to \code{distrSimoptions}.}

\item{print}{\code{signature(x = "Dataclass")}: returns name, filename, the observation dimension, the number of runs 
 and the size of the sample, and, if from a version > 1.8, also the package version under which the object was generated}

\item{print}{\code{signature(x = "Simulation")}: as for \code{signature(x = "Dataclass")} and, additionally,
                   seed and the distribution }

\item{print}{\code{signature(x = "Contsimulation")}:  as for \code{signature(x = "Dataclass")} and, additionally,
              seed, the contamination rate and the distributions}

\item{show}{\code{signature(x = "Dataclass")}, \code{signature(x = "Simulation")},
            \code{signature(x = "Contsimulation"), \code{signature(x = "SeqDataFrames")}}: as corresponding \code{print} method}

}}
\keyword{print}
\keyword{methods}
\concept{S4 simulation class}
\concept{simulation}
