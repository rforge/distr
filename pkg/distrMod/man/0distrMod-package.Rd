\name{distrMod-package}
\alias{distrMod-package}
\alias{distrMod}
\docType{package}

\title{
distrMod -- probability models
}
\description{
Based on the packages \pkg{distr} and \pkg{distrEx} package \pkg{distrMod} 
provides a flexible framework which allows computation of estimators like 
maximum likelihood or minimum distance estimators for probability models.
}

\details{
\tabular{ll}{
Package: \tab distrMod\cr
Version: \tab 2.0 \cr
Date: \tab 2008-01-17 \cr
Depends: \tab R(>= 2.6.1), methods, graphics, startupmsg, distr(>= 1.9), distrEx(>= 1.9)\cr
SaveImage: \tab no\cr
LazyLoad: \tab yes\cr
License: \tab GPL (version 2 or later)\cr
URL: \tab https://r-forge.r-project.org/projects/distrmod/\cr
}}

\section{Classes}{
\preformatted{

ProbFamily classes
slots: [<name>(<class>)] 
name(character), distribution(Distribution),
distrSym(DistributionSymmetry), props(character)

"ProbFamily"
|>"ParamFamily"
additional slots:
param(ParamFamParameter), modifyParam(function)

"ParamFamParameter" is subclass of class "Parameter" of package "distr".
Additional slots:
main(numeric), nuisance(OptionalNumeric), trafo(matrix)

"OptionalNumeric" = union("numeric", "NULL")

Symmetry classes
slots:
type(character), SymmCenter(ANY)

"Symmetry"
|>"NoSymmetry"
|>"EllipticalSymmetry"
|>|>"SphericalSymmetry"
}
}
\section{Methods}{
not specified yet...
}
\author{
Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
Matthias Kohl \email{Matthias.Kohl@stamats.de}\cr

\emph{Maintainer:} Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}
}

\references{
P. Ruckdeschel, M. Kohl, T. Stabla, F. Camphausen (2006):
S4 Classes for Distributions, {\em R News}, {\em 6}(2), 2-6. 
\url{http://CRAN.R-project.org/doc/Rnews/Rnews_2006-2.pdf}

a vignette for packages \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and 
\pkg{distrEx} is included into the mere documentation package \pkg{distrDoc} 
and may be called by 
\code{require("distrDoc");vignette("distr")}
}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting 
\code{options("StartupBanner"="off")} somewhere before loading this 
package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  
\code{options("StartupBanner"="complete")} the complete start-up banner is 
displayed.

For any other value of option \code{"StartupBanner"} 
(i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require} 
call into either \code{suppressStartupMessages()} or 
\code{onlytypeStartupMessages(.,atypes="version")}. 

As for general \code{packageStartupMessage}'s, you may also suppress all
the start-up banner by wrapping the \code{library} or \code{require} 
call into \code{suppressPackageStartupMessages()} from 
\pkg{startupmsg}-version 0.5 on.
}

\section{Demos}{
none so far ...
}
