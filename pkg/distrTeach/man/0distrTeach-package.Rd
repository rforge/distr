\name{distrTeach-package}
\alias{distrTeach-package}
\alias{distrTeach}
\docType{package}
\title{
distrTeach -- Teaching Extensions of Package distr
}
\description{
\pkg{distrTeach} provides some illustrations based on package \pkg{distr}
for teaching Stochastics / Statistics in secondary school; so far the following
has been implemented
\itemize{
\item \code{illustrateLLT}: function for the generation of LLN - visualizations
\item  \code{illustrateCLT}: function for the generation of CLT - visualizations
\item  \code{plotCLT}: Generic function for the plotting of CLT-approximations
}
as well as a Tcl/Tk based demo for\code{illustrateCLT}
}

\details{
\tabular{ll}{
Package: \tab distrTeach \cr
Version: \tab 2.8.0 \cr
Date: \tab 2019-03-11 \cr
Depends: \tab R(>= 2.2.0), methods, distr(>= 2.2), distrEx(>= 2.2) \cr
Suggests: \tab tcltk\cr
Imports: \tab startupmsg, grDevices, graphics, stats\cr
LazyLoad: \tab yes \cr
License: \tab LGPL-3 \cr
URL: \tab http://distr.r-forge.r-project.org/\cr
VCS/SVNRevision: \tab 1314 \cr
}
}
\section{Classes}{
\preformatted{

Teaching Classes

}
}
%\section{Functions}{
%
%\preformatted{
%
%}}
%
%\section{Generating Functions}{
%\preformatted{
%
%}}
\section{Methods}{
\preformatted{

illustration:
illustrateLLT           function for the generation of LLN - visualizations
illustrateCLT           function for the generation of CLT - visualizations
plotCLT                 Generic function for the plotting of CLT-approximations

}
}

\section{Demos}{
Demos are available --- see \code{demo(package="distrTeach")}.}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting 
\code{options("StartupBanner"="off")} somewhere before loading this package by 
\code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or 
\code{options("StartupBanner"="complete")} the complete start-up banner is 
displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in 
\code{c(NULL,"off","complete")}) only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call 
into either \code{suppressStartupMessages()} or 
\code{onlytypeStartupMessages(.,atypes="version")}.  
 
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

\author{
Matthias Kohl \email{Matthias.Kohl@stamats.de} and \cr
Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de},\cr 
Eleonara Feist \email{eleonoragerber@gmx.de}, and, \cr
Anja Hueller \email{anja\_h86@web.de}\cr  

\emph{Maintainer:}  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}
}
\references{
P. Ruckdeschel, M. Kohl, T. Stabla, F. Camphausen (2006):
S4 Classes for Distributions, \emph{R News}, \emph{6}(2), 2-6. 
\url{https://CRAN.R-project.org/doc/Rnews/Rnews_2006-2.pdf}

%a more detailed manual for \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and \pkg{distrTeach} may be downloaded from  
%\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf}\cr

a vignette for packages \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, 
and \pkg{distrTeach} is included into the mere documentation package \pkg{distrDoc} 
and may be called by \code{require("distrDoc");vignette("distr")}

a homepage to this package is available under\cr
\url{http://distr.r-forge.r-project.org/} and the pages ...

M. Kohl (2005): \emph{Numerical Contributions to the Asymptotic 
Theory of Robustness.} PhD Thesis. Bayreuth. Available as 
\url{http://www.stamats.de/wp-content/uploads/2018/04/ThesisMKohl.pdf}
}
\keyword{package}
\concept{LLN}
\concept{CLT}
\concept{Teaching demos}
\seealso{
\code{\link[distr:0distr-package]{distr-package}} 
\code{\link[distrEx:0distrEx-package]{distrEx-package}} 
}
