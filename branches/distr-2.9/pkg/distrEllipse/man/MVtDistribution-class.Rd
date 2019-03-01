\name{MVtDistribution-class}
\docType{class}
\alias{MVtDistribution-class}
\alias{sigma,MVtDistribution-method}
\alias{df,MVtDistribution-method}
\alias{ncp,MVtDistribution-method}


\title{MVt distribution class}
\description{
Class \code{MVtDistribution} implements multivariate t distributions using
code from package \pkg{mvtnorm}. For details to this implementation confer
to the references given in this package.
}
\section{Objects from the Class}{
  Objects could in principle be created by calls to \code{new}, but more 
  frequently you would create them  via the generating function 
  \code{\link{MVtDistribution}}.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}. }
    \item{\code{param}:}{Object of class \code{"MVtParameter"}. }
    \item{\code{r}:}{function with argument \code{n}; random number generator}
    \item{\code{d}:}{ the density of this distribution, \code{\link[mvtnorm:Mvt]{dmvt}}}
    \item{\code{p}:}{the (vectorized) function \code{\link[mvtnorm]{pmvt}}.}
    \item{\code{q}:}{the (vectorized) function \code{\link[mvtnorm]{qmvt}}.}
    \item{\code{radDistr}:}{an object of class \code{AbscontDistribution}
    with density 
    \deqn{{\rm dim} {({\rm dim}+{\rm df}-1)/2\choose{{\rm df}/2-1}} x^{{\rm dim}-1} {\rm df}^{-{\rm dim}/2}/
      (1+x^2/{\rm df})^{({\rm dim}+{\rm df})/2}}{
      dim * choose((dim+df-1)/2, (df-1)/2) * x ^{dim-1} *  df^(-dim/2) * 
      (1+x^2/df)^{-(dim+df)/2}}
    }
    \item{\code{.withArith}:}{FALSE}
    \item{\code{.withSim}:}{FALSE}
    \item{\code{.logExact}:}{TRUE}
    \item{\code{.lowerExact}:}{TRUE}
    \item{\code{Symmetry}:}{object of class \code{"EllipticalSymmetry"} about 
    center \code{loc}; used internally to avoid unnecessary calculations.}
}}
\section{Extends}{
Class \code{"EllipticalDistribution"}, directly. \cr
Class \code{"SphericalDistribution"}, by class \code{"EllipticalDistribution"}. \cr
Class \code{"MultivariateDistribution"}, by class \code{"SphericalDistribution"}.
Class \code{"Distribution"}, by class \code{"MultivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{sigma}{\code{signature(object = "MVtDistribution")}: wrapped access method for
      slot \code{sigma} of slot \code{param}. }

    \item{ncp}{\code{signature(object = "MVtDistribution")}: wrapped access method for
      slot \code{ncp} of slot \code{param}. }

    \item{df}{\code{signature(x = "MVtDistribution")}: wrapped access method for
      slot \code{scale} of slot \code{param}. }


  }
}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\examples{
new("MVtDistribution") ## better use generating function MVtDistribution()
}
\seealso{Package \pkg{mvtnorm}}
\concept{Multivariate t Distribution}
\keyword{distribution}
\concept{extreme value distribution}
\concept{absolutely continuous distribution}
\concept{S4 distribution class}
