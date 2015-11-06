\name{distrEllipseoptions}
\alias{distrEllipseoptions}
\alias{getdistrEllipseOption}
\alias{Nsim}
\alias{withED}
\alias{lwd.Ed}
\alias{col.Ed}
\alias{withMean}
\alias{cex.mean}
\alias{pch.mean}
\alias{col.mean}


\title{
  functions to change the global variables of the package `distrEllipse'
}
\description{
  With \code{distrEllipseoptions} and \code{getdistrEllipseOption} you may
  inspect and change the global variables used by package \pkg{distrEllipse}.
}
\usage{
distrEllipseoptions(...)
getdistrEllipseOption(x)
}
\arguments{
  \item{\dots}{ any options can be defined, using name = value or by passing a list of such tagged values. }
  \item{x}{ a character string holding an option name.}
}
\details{Invoking \code{distrEllipseoptions()} with no arguments returns a list with the current values of the options.
 To access the value of a single option, one should use \code{getdistrEllipseOption("WarningSim")}, e.g., rather than
 \code{distrEllipseoptions("WarningSim")} which is a \emph{list} of length one.}
\value{
\code{distrEllipseoptions()} returns a list of the global options of \pkg{distrEllipse}. \cr
\code{distrEllipseoptions("Nsim")} returns  the global option \code{Nsim} as a list of length 1. \cr
\code{distrEllipseoptions("Nsim" = 3000)} sets the value of the global option  \code{Nsim} to 3000.
\code{getdistrEllipseOption("Nsim")} the current value set for option \code{Nsim}.
}
\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}
  }

\section{Currently available options}{
\describe{
  \item{\code{Nsim}}{ for plotting: number of (simulated) points to be plotted.}

  \item{\code{withED}}{ for plotting: logical; shall principal axes of the contour ellipsoid be plot in
                 (for each panel)?}
  \item{\code{lwd.Ed}}{ for plotting: line width of principal axes  (for each panel).}
  \item{\code{col.Ed}}{ for plotting: color of principal axes  (for each panel).}
  \item{\code{withMean}}{ for plotting: logical; shall mean  be plot in  (for each panel)?}
  \item{\code{cex.mean}}{ for plotting: size of the mean symbol  (for each panel).}
  \item{\code{pch.mean}}{ for plotting: mean symbol  (for each panel).}
  \item{\code{col.mean}}{ for plotting: color of the mean symbol (for each panel).}

  }
}
\examples{
distrEllipseoptions("Nsim") # returns the value of Nsim, by default = 5
currentDistrOptions <- distrEllipseoptions()
distrEllipseoptions(Nsim = 6000)
distrEllipseoptions("Nsim")
getdistrEllipseOption("Nsim")
distrEllipseoptions(c("Nsim","withED"))
}
\seealso{\code{\link[base]{options}}, \code{\link[base:options]{getOption}}}
\keyword{distribution}
\concept{global options}
\concept{options}
