\name{distrDoc-package}
\alias{distrDoc-package}
\alias{distrDoc}
\docType{package}
\title{
distrDoc -- Documentation for 'distr' Family of R Packages
}
\description{
\pkg{distrDoc} provides documentation in form of a common vignette to 
packages \pkg{distr}, \pkg{distrEx}, \pkg{distrSim}, \pkg{distrTEst},
\pkg{distrTeach}, \pkg{distrMod}, and \pkg{distrEllipse}.
}
\details{
\tabular{ll}{
Package: \tab distrDoc \cr
Version: \tab 2.6 \cr
Date: \tab 2015-05-03 \cr
Depends: \tab R(>= 2.14.0)\cr
Imports: \tab distr(>= 2.2.0), distrEx(>= 2.2.0), distrSim(>= 2.2.0), distrTEst(>= 2.2.0),
        distrTeach(>= 2.2.0), RandVar(>= 0.7), distrMod(>= 2.2.0), MASS, methods,
        SweaveListingUtils, startupmsg \cr
Suggests: \tab Biobase \cr
ByteCompile: \tab yes \cr
License: \tab LGPL-3 \cr
URL: \tab http://distr.r-forge.r-project.org/\cr
SVNRevision: \tab 926 \cr
}}


\section{Acknowledgement}{
The idea to split off a common vignette for a set of packages 
 in a particular documentation package is due to Seth Falcon.}
\author{
Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
Thomas Stabla \email{statho3@web.de},\cr 
Florian Camphausen \email{fcampi@gmx.de},\cr
Matthias Kohl \email{Matthias.Kohl@stamats.de}\cr

\emph{Maintainer:} Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
}

\seealso{see mails "[Rd] package vignettes, suggests and R CMD check"
         on r-devel by Seth Falcon,\cr
         \url{https://stat.ethz.ch/pipermail/r-devel/2006-November/043702.html},\cr
         \url{https://stat.ethz.ch/pipermail/r-devel/2006-November/043705.html},\cr
         and by the author,\cr  
         \url{https://stat.ethz.ch/pipermail/r-devel/2006-November/043700.html},\cr
         \url{https://stat.ethz.ch/pipermail/r-devel/2006-November/043703.html}\cr
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
either \code{suppressStartupMessages()} or \code{onlytypeStartupMessages(.,atypes="version")}.  
 
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
\keyword{package}
\keyword{utilities}
\keyword{documentation}
\concept{vignette}
\concept{S4 distribution class}
\concept{distr familiy}
\examples{
\dontrun{
## Open the distr vignette
     vignette("distr")
}
}
