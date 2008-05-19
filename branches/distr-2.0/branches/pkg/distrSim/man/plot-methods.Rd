\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,Dataclass-method}
\alias{plot,Simulation-method}
\alias{plot,Contsimulation-method}
\title{ Methods for Function plot in Package `distrSim' }
\description{plot-methods}
\section{Methods}{\describe{
\item{plot}{\code{signature(x = "Dataclass")}: produces a plot of the data matrix;
optional arguments: 
\describe{
\item{obs0}{the indices of observations to be plotted;--- of this vector \code{runs0} 
maximally \code{MaxNumberofPlottedObs} runs are plotted where \code{MaxNumberofPlottedObs} 
is a global option, see \code{\link[distrSim]{distrSimoptions}}}
\item{runs0}{the indices of runs to be plotted; --- of this vector \code{runs0} 
maximally \code{MaxNumberofPlottedRuns} runs are plotted where \code{MaxNumberofPlottedRuns} 
is a global option, see \code{\link[distrSim]{distrSimoptions}}}
\item{dims0}{the indices of observation dimensions to be plotted; --- of this vector \code{dims0} 
maximally \code{MaxNumberofPlottedObsDims} dimensions are plotted where \code{MaxNumberofPlottedObsDims} 
is a global option, see \code{\link[distrSim]{distrSimoptions}}}
}
typical plot arguments may be passed; some have slightly different meaning (compare \code{\link[graphics]{plot.default}}):
\describe{
\item{ylim}{may be transmitted matrixwise (in a 2 x (number of dimensions) matrix)) or globally, using the usual recycling rules}
\item{col,cex,pch}{may be transmitted vectorwise (for the different dimensions) or globally, using the usual recycling rules}
\item{xlab,ylab,type}{are ignored}
}}
\item{plot}{\code{signature(x = "Simulation")}: 
produces a plot of the data matrix; optional and typical plot arguments: as with \code{signature(object = "Dataclass")}
 }
\item{plot}{\code{signature(x = "Contsimulation")}: produces a plot of the real data matrix; 
optional arguments: as with \code{signature(x = "Dataclass")};
typical plot arguments may be passed; some have slightly different meaning (compare \code{\link[graphics]{plot.default}}):
\describe{
\item{ylim}{as with \code{signature(x = "Simulation")} and \code{signature(object = "Dataclass")} }
\item{col,cex,pch}{are ignored}
\item{col.id,cex.id,pch.id}{as \code{col,cex,pch} for \code{signature(object = "Dataclass")} but only for ideal part of the data}
\item{col.c,cex.c,pch.c}{as \code{col,cex,pch} for \code{signature(object = "Dataclass")} but only for contaminated part of the data}
\item{xlab,ylab,type}{are ignored}
}
 }
}}
\keyword{methods}
\keyword{hplot}
\concept{simulation}
\concept{S4 simulation class}
