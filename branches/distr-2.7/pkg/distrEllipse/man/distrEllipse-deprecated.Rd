\name{distrEllipse-deprecated}
\alias{distrEllipse-deprecated}
\alias{plot.rd}
\alias{r.rd}
\alias{d.rd}
\alias{p.rd}
\alias{q.rd}
\title{Deprecated Functions in Package \pkg{distrEllipse}}
\description{
  These functions are provided for compatibility with older versions of
  \pkg{distrEllipse} only, and may be defunct as soon as the next release.
}
\section{Methods}{
  From version 2.6 on, we deprecate former versions of S4-methods
  \code{rRd}, \code{dRd}, \code{pRd}, \code{qRd}, and \code{plotRd} of style
  <name>.rd due to clashes with S3-method inheritance. 
  More specifically, this concerns the following methods:
  \describe{
    \item{r.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{r} of slot \code{radDistr}.}
    \item{d.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{d} of slot \code{radDistr}. }
    \item{p.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{p} of slot \code{radDistr}. }
    \item{q.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{q} of slot \code{radDistr}. }
    \item{plot.rd}{\code{signature(x = "SphericalDistribution")}: utility; calls \code{plot}
      for slot \code{radDistr}. }
  }  
}

\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
