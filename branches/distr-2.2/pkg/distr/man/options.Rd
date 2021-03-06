\name{options}
\alias{options}
\alias{newDevice}

\title{
  additional options in package `distr'
}
\description{
  In package \pkg{distr}, we add an extra option \code{"newDevice"};
  it is inspected and manipulated as usual.
}
\details{We do not change the behaviour of \code{options} or \code{getOption};
for the general documentation to these two functions, 
confer \code{\link[base]{options}}, \code{\link[base:options]{getOption}}. Here
we only document added options.}
\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}}

\section{Additionally available options in package 'distr'}{
\describe{
\item{\code{"newDevice"}}{logical; controls behaviour when generating several plots within
one function; if \code{TRUE} a call to \code{\link{devNew}} is inserted to hook function
\code{\link[graphics:frame]{plot.new}}; if \code{FALSE}, we reproduce the usual behaviour 
in \pkg{graphics}, i.e.; we do not call \code{\link{devNew}}. Defaults to \code{FALSE}.}
}
} 
\examples{
getOption("newDevice") 
options("newDevice"=TRUE) 
}
\seealso{\code{\link[base]{options}}, \code{\link[base:options]{getOption}}}
\keyword{environment}
\keyword{iplot}
\keyword{dplot}
\concept{global options}
\concept{options}
