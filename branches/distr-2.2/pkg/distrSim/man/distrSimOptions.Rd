\name{distrSimoptions}
\alias{distrSimoptions}
\alias{getdistrSimOption}
\alias{MaxNumberofPlottedObsDims}
\alias{MaxNumberofPlottedRuns}
\alias{MaxNumberofSummarizedObsDims}
\alias{MaxNumberofSummarizedRuns}

\title{
  functions to change the global variables of the package `distrSim'
}
\description{
  With \code{distrSimoptions} and \code{getdistrSimOption} you may inspect and change the global
  variables used by package \pkg{distrSim}.
}
\usage{
distrSimoptions(...)
getdistrSimOption(x)
}
\arguments{
  \item{\dots}{ any options can be defined, using name = value or by passing a list of such tagged values. }
  \item{x}{ a character string holding an option name.}
}
\details{Invoking \code{distrSimoptions()} with no arguments returns a list with the current values of the options.  
 To access the value of a single option, one should use \code{getdistrSimOption("MaxNumberofSummarizedRuns")}, 
 e.g., rather than \code{distroptions("MaxNumberofSummarizedRuns")} which is a \emph{list} of length one.}
\value{
\code{distrSimoptions()} returns a list of the global options of \pkg{distrSim}. \cr
\code{distrSimoptions("MaxNumberofSummarizedRuns")} returns  the global option \code{MaxNumberofSummarizedRuns} as a list of length 1. \cr
\code{distrSimoptions("MaxNumberofSummarizedRuns" = 3)} sets the value of the global option  \code{MaxNumberofSummarizedRuns} to 3. 
\code{getdistrSimOption("MaxNumberofSummarizedRuns")} the current value set for option \code{MaxNumberofSummarizedRuns}. 
}

\section{Currently available options}{
\describe{
  \item{MaxNumberofPlottedObs}{ maximal number of observation plotted; defaults to 4000 }
  \item{MaxNumberofPlottedObsDims}{ maximal number of observation dimensions plotted in parallel; defaults to 6}
  \item{MaxNumberofPlottedRuns}{ maximal number of runs plotted in parallel; defaults to 6 }
  \item{MaxNumberofSummarizedObsDims}{ maximal number of observation dimensions summarized in parallel; defaults to 6 }
  \item{MaxNumberofSummarizedRuns}{ maximal number of runs summarized in parallel; defaults to 6 }
}
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link[base]{options}}, \code{\link[base]{getOption}},
\code{\link[distr]{distroptions}}, \code{\link[distr]{getdistrOption}}}
\examples{
distrSimoptions()
distrSimoptions("MaxNumberofPlottedObsDims")
distrSimoptions("MaxNumberofPlottedObsDims" = 5)
# or
getdistrSimOption("MaxNumberofPlottedObsDims")
}
\keyword{misc}
\concept{global options}
\concept{options}
