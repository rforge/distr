\name{distrRmetrics-package}
\alias{distrRmetrics-package}
\alias{distrRmetrics}
\docType{package}
\title{
distrRmetrics -- Distribution Classes for Distributions from Rmetrics.
}
\description{
\pkg{distrRmetrics} provides infrastructure / (S4-)classes
(based on package \pkg{distr}) for distributions contributed in the Rmetrics
packages.
}
\details{
\tabular{ll}{
Package: \tab distrRmetrics \cr
Version: \tab 2.8.0 \cr
Date: \tab 2019-03-11 \cr
Depends: \tab R(>= 2.6.0), methods, distr(>= 2.4), fBasics(>= 270.73), fGarch(>= 270.73) \cr
Suggests: \tab distrEx(>= 2.4), distrMod(>= 2.4)  \cr
Imports: startupmsg  \cr
ByteCompile: \tab yes \cr
License: \tab LGPL-3 \cr
URL: \tab http://distr.r-forge.r-project.org/\cr
VCS/SVNRevision: \tab 1314 \cr
}
}
\section{Classes}{
\preformatted{
###################################
Distribution Classes
###################################
[*]: there is a generating function with the same name


"Distribution" (from distr)
|>"AbscontDistribution" (from distr)
|>|>"SNorm" [*]
|>|>"SSTd" [*]
}}


\section{Functions}{
\preformatted{
STd    Functions to generate an "AbscontDistribution" object implementing
       a standardized T distribution
}}

\section{Slot accessors / -replacement functions}{
All slots are inspected / modified by corresponding 
accessors / -replacement functions.
}

\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de},\cr
  \emph{Maintainer:}  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}
  }

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call into
either \code{\link[startupmsg:StartupUtilities]{suppressStartupMessages}()} or 
\code{\link[startupmsg:StartupUtilities]{onlytypeStartupMessages}(.,atypes="version")}. 

As for general \code{packageStartupMessage}'s, you may also suppress all
 the start-up banner by wrapping the \code{library} or \code{require}
 call into \code{suppressPackageStartupMessages()} from
 \pkg{startupmsg}-version 0.5 on.
}



\section{Package versions}{
Note: The first two numbers of package versions do not necessarily reflect
 package-individual development, but rather are chosen for the
 distrXXX family as a whole in order to ease updating "depends"
 information.
}

\references{
P. Ruckdeschel, M. Kohl, T. Stabla, F. Camphausen (2006):
S4 Classes for Distributions, \emph{R News}, \emph{6}(2), 2-6. 
\url{https://CRAN.R-project.org/doc/Rnews/Rnews_2006-2.pdf}

A vignette for packages \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, \pkg{distrEx},
\pkg{distrTeach}, \pkg{distrMod}, and \pkg{distrRmetrics}
is included into the mere documentation package \pkg{distrDoc} and may be called by 
\code{require("distrDoc");vignette("distr")}.

A homepage to this package is available under\cr
\url{http://distr.r-forge.r-project.org/}.
}

\keyword{ package }
