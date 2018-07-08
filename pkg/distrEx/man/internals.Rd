\name{internals_for_distrEx}
\alias{internals_for_distrEx}
\alias{.getIntbounds}

\title{Internal functions of package distrEx}

\description{
These functions are used internally by package distrEx.}

\usage{
.getIntbounds(object, low, upp, lowTQ, uppTQ, IQR.fac, ...)
}


\arguments{
  \item{object}{an object of class \code{"AbscontDistribution"}}
  \item{low}{given lower integration bound}
  \item{upp}{given lower integration bound}
  \item{lowTQ}{lower quantile for quantile based integration range.}
  \item{uppTQ}{upper quantile for quantile based integration range.}
  \item{IQR.fac}{factor for scale based integration range (i.e.; 
  median of the distribution \eqn{\pm}{+-}\code{IQR.fac}\eqn{\times}{*}IQR).}
 \item{\dots}{arguments passed through to other functions (in particular argument
  \code{cond} in \code{.getIntbounds})}
}

\details{
\code{.getIntbounds} integration bounds are obtained
as \code{lowB <- max(low, q.l(object)(lowTQ), median(object)-IQR.fac*IQR(object)}
and \code{uppB <- min(upp, q.l(object)(1-uppTQ), median(object)+IQR.fac*IQR(object)}
}


\value{
\item{.getIntbounds}{a named numeric vector with coordinates \code{low} and \code{upp}.}
}

\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de},
}

\seealso{
\code{\link[distr]{AbscontDistribution}},
\code{\link{distrExIntegrate}}
}
\keyword{internal}
\concept{utilities}
