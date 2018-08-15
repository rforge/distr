\name{internals_for_distrEx}
\alias{internals_for_distrEx}
\alias{.getIntbounds}
\alias{.filterFunargs}
\alias{.filterEargs}

\title{Internal functions of package distrEx}

\description{
These functions are used internally by package distrEx.}

\usage{
.getIntbounds(object, low, upp, lowTQ, uppTQ, IQR.fac, ...)
.filterFunargs(dots, fun, neg = FALSE)
.filterEargs(dots, neg = FALSE)
}


\arguments{
  \item{object}{an object of class \code{"AbscontDistribution"}}
  \item{low}{given lower integration bound}
  \item{upp}{given lower integration bound}
  \item{lowTQ}{lower quantile for quantile based integration range. }
  \item{uppTQ}{upper quantile for quantile based integration range. }
  \item{IQR.fac}{factor for scale based integration range (i.e.; 
  median of the distribution \eqn{\pm}{+-}\code{IQR.fac}\eqn{\times}{*}IQR).}
 \item{dots}{ a list as obtained by \code{list(...)}. }
 \item{\dots}{arguments passed through to other functions (in particular argument
  \code{cond} in \code{.getIntbounds})}
  \item{neg}{logical: if \code{FALSE} (default) the items in \code{dots} with argument
     names coinciding with formal argument names used by \code{distrExIntegrate},
     \code{integrate}, \code{GLIntegrate}, except for \code{\dots}, \code{f},
     \code{distr}, are returned; otherwise the logical complement, i.e., those
     items not coinciding with the mentioned names are returned.}
\item{fun}{ a function the formals of which are to be filtered from argument list
  \code{dots}. }
}

\details{
\code{.getIntbounds} integration bounds are obtained
as \code{lowB <- max(low, q.l(object)(lowTQ), median(object)-IQR.fac*IQR(object)}
and \code{uppB <- min(upp, q.l(object)(1-uppTQ), median(object)+IQR.fac*IQR(object)}

\code{.filterEargs} filters out arguments that are used in numerical integration
   and returns only these (or returns all items except for them, if argument
   \code{neg} is \code{TRUE}).

\code{.filterFunargs} filters out arguments the names of which are within the
   names of the formal arguments of \code{fun} (or returns all items of
   \code{dots} except for the matched ones, if argument
   \code{neg} is \code{TRUE}).
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
