\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,Dataclass-method}
\alias{summary,Simulation-method}
\alias{summary,Contsimulation-method}
\title{ Methods for Function summary in Package 'distrSim' }
\description{summary-methods}

\section{Methods}{
\describe{
\item{summary}{\code{signature(object = "Dataclass")}: returns the same information as print, 
    moreover a statistical summary for each run; optional arguments: 
    \describe{
    \item{runs0}{the indices of runs to be summarized; --- of this vector \code{runs0} 
        maximally \code{MaxNumberofSummarizedRuns} runs are summarized where \code{MaxNumberofSummarizedRuns} 
        is a global option, see \code{\link[distrSim]{distrSimoptions}}}
    \item{dims0}{the indices of observation dimensions to be summarized; --- of this vector \code{dims0} 
        maximally \code{MaxNumberofSummarizedObsDims} dimensions are summarized where \code{MaxNumberofSummarizedObsDims} 
        is a global option, see \code{\link[distrSim]{distrSimoptions}}}
    }}

\item{summary}{\code{signature(object = "Simulation")}: returns name, filename,
seed, number of runs, the size of the sample and a
    statistical summary for each run; optional arguments: as with \code{signature(object = "Dataclass")} }

\item{summary}{\code{signature(object = "Contsimulation")}: returns name, filename,
seed, number of runs, the size of the sample, the
    rate and a statistical summary for each run of the real data; optional arguments:
    as with \code{signature(object = "Dataclass")} }
}}
\concept{summary}
\keyword{methods}
\concept{S4 data class}
\concept{S4 simulation class}
