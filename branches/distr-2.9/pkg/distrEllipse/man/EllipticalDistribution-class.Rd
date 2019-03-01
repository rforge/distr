\name{EllipticalDistribution-class}
\docType{class}
\alias{EllipticalDistribution-class}
\alias{scale,EllipticalDistribution-method}
\alias{location,EllipticalDistribution-method}
\alias{scale<-,EllipticalDistribution-method}
\alias{location<-,EllipticalDistribution-method}
\alias{E,EllipticalDistribution,missing,missing-method}
\alias{E,EllipticalDistribution,function,missing-method}
\alias{var,EllipticalDistribution-method}
\alias{+,EllipticalDistribution,numeric-method}
\alias{*,EllipticalDistribution,numeric-method}
\alias{\%*\%,matrix,EllipticalDistribution-method}
\alias{coerce,EllipticalDistribution,UnivariateDistribution-method}
\alias{coerce,UnivariateDistribution,EllipticalDistribution-method}

\title{Elliptical distribution class}
\description{
Class \code{EllipticalDistribution} implements general elliptically symmetric
distributions, i.e. starting from a spherically distribution realized as an 
object \code{S} of class \code{\link{SphericalDistribution}}, this is the 
distribution of an affine linear transformation \code{AS+b}. 
}
\section{Objects from the Class}{
  Objects could in principle be created by calls to \code{new}, but more 
  frequently you would create them  via the generating function 
  \code{\link{EllipticalDistribution}}.
}
\section{Slots}{
  \describe{
    \item{\code{img}}{Object of class \code{"Reals"}. }
    \item{\code{param}}{Object of class \code{"EllipticalParameter"}. }
    \item{\code{r}}{function with argument \code{n}; random number generator}
    \item{\code{d}}{optional function; in case it exists:
    the density of the distribution}
    \item{\code{p}}{optional function; in case it is non-null:
    the cdf of the distribution evaluated on rectangles, i.e. if a random
    variable \code{X} is distributed according to an \code{object} of class
    \code{"EllipticalDistribution"}, 
    for \code{q} a matrix of dimension \eqn{d \times n}{d x n} \code{p(object)(q)}
    returns, for each of the \code{n} columns 
    \eqn{P(X_i\leq q_i,\;i=1,\ldots,d)}{P(X[i]<= q[i],\;i=1,\dots,d)}.}
    \item{\code{q}}{optional function; in case it is non-null:
    the quantile of the distribution evaluated on rectangles, i.e. if a random
    variable \code{X} is distributed according to an \code{object} of class
    \code{"EllipticalDistribution"}, 
    for \code{p} a vector of length  \eqn{n}, returns, for each of the 
    \code{n} components the infinimal number \eqn{q_j}{q[j]} such that
    \eqn{P(X_i\leq q_j,\;i=1,\ldots,d)\ge p_j}{P(X[i]<= q[j],\;i=1,\dots,d)>= p[j]}.}
    \item{\code{radDistr}}{an object of class \code{UnivariateDistribution} with positive 
                  support, i.e. \code{p(radDistr)(0)==0}; the radial distribution. }
    \item{\code{.withArith}}{logical: used internally to issue warnings as to 
            interpretation of arithmetics}
    \item{\code{.withSim}}{logical: used internally to issue warnings as to 
          accuracy}
    \item{\code{.logExact}}{logical: used internally to flag the case where 
    there are explicit formulae for the log version of density, cdf, and 
    quantile function}
    \item{\code{.lowerExact}}{logical: used internally to flag the case where 
    there are explicit formulae for the lower tail version of cdf and quantile 
    function}
    \item{\code{Symmetry}}{object of class \code{"EllipticalSymmetry"} about 
    center \code{loc}; used internally to avoid unnecessary calculations.}
}
}
\section{Extends}{
Class \code{"SphericalDistribution"}, directly. \cr
Class \code{"MultivariateDistribution"}, by class \code{"SphericalDistribution"}.
Class \code{"Distribution"}, by class \code{"MultivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{location}{\code{signature(object = "EllipticalDistribution")}: wrapped access method for
      slot \code{location} of slot \code{param}. }

    \item{scale}{\code{signature(x = "EllipticalDistribution")}: wrapped access method for
      slot \code{scale} of slot \code{param}. }

    \item{location<-}{\code{signature(object = "EllipticalDistribution")}: wrapped replace method for
      slot \code{location} of slot \code{param}. }

    \item{scale<-}{\code{signature(x = "EllipticalDistribution")}: wrapped replace method for
      slot \code{scale} of slot \code{param}. }

    \item{E}{\code{signature(object = "EllipticalDistribution", fun = "missing", cond = "missing")}: 
      expectation of an elliptically symmetric distribution; exact.
    }
    \item{E}{\code{signature(object = "EllipticalDistribution", fun = "function", cond = "missing")}: 
      expectation of an elliptically symmetric distribution; by simulation.
    }
    \item{var}{\code{signature(x = "EllipticalDistribution")}: 
      expectation of an elliptically symmetric distribution; exact.
    }
    \item{\code{+}}{\code{signature(e1 = "EllipticalDistribution", e2 = "numeric")}:
      affine linear transformation; exact.
    } 
    \item{\code{-}}{\code{signature(e1 = "EllipticalDistribution", e2 = "numeric")}:
      affine linear transformation; exact.
    } 
    \item{\code{*}}{\code{signature(e1 = "EllipticalDistribution", e2 = "numeric")}:
      affine linear transformation; exact.
    } 
    \item{\code{\%*\%}}{\code{signature(e1 = "numeric", e2 = "EllipticalDistribution")}:
      affine linear transformation; exact.
    } 
    \item{coerce}{\code{signature(from = "EllipticalDistribution", to = "UnivariateDistribution")}:
      create a \code{UnivariateDistribution} object from a (one-dimensional) 
      elliptically symmetric distribution. }
    \item{coerce}{\code{signature(from = "UnivariateDistribution", to = "EllipticalDistribution")}:
      create a \code{EllipticalDistribution} object from a (symmetric) 
      univariate distribution. }
  }
}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\examples{
new("EllipticalDistribution") ## better use EllipticalDistribution()
}
\concept{Elliptical Distribution}
\keyword{distribution}
\concept{extreme value distribution}
\concept{absolutely continuous distribution}
\concept{S4 distribution class}
