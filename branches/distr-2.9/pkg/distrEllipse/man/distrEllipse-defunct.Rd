\name{distrEllipse-defunct}
\alias{distrEllipse-defunct}
\alias{plot.rd}
\alias{r.rd}
\alias{d.rd}
\alias{p.rd}
\alias{q.rd}
\title{Defunct Functions in Package \pkg{distrEllipse}}
\description{
  Functions which are no longer provided in  \pkg{distrEllipse} due to
  clashes with S3-method inheritance.
 }
\section{Methods}{
  From version 2.7 on, former versions of S4-methods
  \code{rRd}, \code{dRd}, \code{pRd}, \code{qRd}, and \code{plotRd} of style
  <name>.rd are defunct due to clashes with S3-method inheritance.
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
  \code{\link{Defunct}}
}
\keyword{misc}
