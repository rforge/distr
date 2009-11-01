\name{distrEllipse-package}
\alias{distrEllipse-package}
\alias{distrEllipse}
\docType{package}
\title{
distrEllipse -- S4 classes for elliptically contoured distributions
}
\description{
\pkg{distrEllipse} provides infrastructure / (S4-)classes for elliptically
contoured distributions (based on package distr).
}
\details{
\tabular{ll}{
Package: \tab distrEllipse \cr
Version: \tab 2.2 \cr
Date: \tab 2009-09-04 \cr
Depends: \tab R(>= 2.8.0), methods, graphics, mvtnorm, setRNG(>= 2006.2-1),
              distr(>= 2.2), distrEx(>= 2.2), distrSim(>= 2.2), startupmsg\cr
LazyLoad: \tab yes \cr
License: \tab LGPL-3 \cr
URL: \tab http://distr.r-forge.r-project.org/\cr
}
}
\section{Classes}{
\preformatted{
###################################
Distribution Classes
###################################
[*]: there is a generating function with the same name


"Distribution" (from distr)
|>"MultivariateDistribution" (from distrEx)
|>|>"MultivarMixingDistribution" [*]
|>|>"SphericalDistribution" [*]
|>|>|>|>"EllipticalDistribution" [*]
|>|>|>|>"MVNormDistribution" [*]

"DistrList" (from distr)
|>"MultivarDistrList" [*/class union of "MVDistrList", "UnivarDistrList"]
|>|>"MVDistrList"
|>"UnivarDistrList" (from distr) [*]
}}

\section{Methods}{
\preformatted{
plot-methods            Methods for Function plot
                       (for SphericalDistribution)
show-methods            Methods for Function show
                       (for Simulation/Contsimulation)
}}

\section{Functions}{
\preformatted{
distrEllipseoptions            Functions to change the global variables of the
                        package 'distrEllipse'
}}

\section{Slot accessors / -replacement functions}{
All slots are inspected / modified by corresponding 
accessors / -replacement functions.
}

\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr
  \emph{Maintainer:}  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
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
}


\note{Global options controlling the plots and summaries of Dataclass and Simulation/Contsimulation 
objects may be inspected / set by \code{\link{distrEllipseoptions}()}
and \code{\link{getdistrEllipseOption}()}.
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
\url{http://CRAN.R-project.org/doc/Rnews/Rnews_2006-2.pdf}

A vignette for packages \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, \pkg{distrEx},
\pkg{distrTeach}, \pkg{distrMod}, and \pkg{distrEllipse}
is included into the mere documentation package \pkg{distrDoc} and may be called by 
\code{require("distrDoc");vignette("distr")}.

%A more detailed manual for \pkg{distr}, \pkg{distrEllipse}, \pkg{distrTEst}, and \pkg{distrEx} may be downloaded from
%\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf}.

A homepage to this package is available under\cr
\url{http://distr.r-forge.r-project.org/}.
}
\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

As for general \code{packageStartupMessage}'s, you may also suppress all
 the start-up banner by wrapping the \code{library} or \code{require} 
 call into \code{suppressPackageStartupMessages()} from 
 \pkg{startupmsg}-version 0.5 on.
 }


\keyword{ package }
\concept{elliptically symmetric distribution}
