\name{distrDoc-package}
\alias{distrDoc-package}
\alias{distrDoc}
\docType{package}
\title{
distrDoc -- documentation for packages distr, distrEx, distrSim, distrTEst
}
\description{
\pkg{distrDoc} provides documentation in form of a common vignette to 
packages \pkg{distr}, \pkg{distrEx}, \pkg{distrSim}, \pkg{distrTEst}.
}
\details{
\tabular{ll}{
Package: \tab distrDoc\cr
Version: \tab 2.0 \cr
Date: \tab 2008-01-15 \cr
Depends: \tab R(>= 2.2.0), tools, distr(>= 1.8.0), startupmsg\cr
Suggests: \tab distrEx(>= 0.4-4), distrSim(>= 1.8.0), distrTEst(>= 1.8.0), Biobase\cr
SaveImage: \tab no\cr
LazyLoad: \tab yes\cr
License: \tab GPL (version 2 or later)\cr
URL: \tab http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/\cr
}}


\section{Acknowledgement}{
The idea to split off a common vignette for a set of packages 
 in a particular documentation package is due to Seth Falcon.}
\author{
Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
Thomas Stabla \email{statho3@web.de},\cr 
Florian Camphausen \email{fcampi@gmx.de},\cr
Matthias Kohl \email{Matthias.Kohl@stamats.de}\cr

\emph{Maintainer:} Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}
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
