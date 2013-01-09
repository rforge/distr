\name{SphericalDistribution-class}
\docType{class}
\alias{SphericalDistribution-class}
\alias{scale,SphericalDistribution-method}
\alias{location,SphericalDistribution-method}
\alias{dimension,SphericalDistribution-method}
\alias{dim,SphericalDistribution-method}
\alias{radDistr,SphericalDistribution-method}
\alias{radDistr<-,SphericalDistribution-method}
\alias{plot.rd,SphericalDistribution-method}
\alias{r.rd,SphericalDistribution-method}
\alias{d.rd,SphericalDistribution-method}
\alias{p.rd,SphericalDistribution-method}
\alias{q.rd,SphericalDistribution-method}
\alias{radDistr}
\alias{radDistr<-}
\alias{plot.rd}
\alias{r.rd}
\alias{d.rd}
\alias{p.rd}
\alias{q.rd}
\alias{E,SphericalDistribution,missing,missing-method}
\alias{var,SphericalDistribution-method}
\alias{coerce,SphericalDistribution,EllipticalDistribution-method}
\alias{show,SphericalDistribution-method}
\alias{showobj,SphericalDistribution-method}
\alias{+,SphericalDistribution,numeric-method}
\alias{*,SphericalDistribution,numeric-method}
\alias{\%*\%,matrix,SphericalDistribution-method}
\alias{+,numeric,SphericalDistribution-method}
\alias{*,numeric,SphericalDistribution-method}
\alias{-,SphericalDistribution,missing-method}
\alias{-,SphericalDistribution,numeric-method}
\alias{-,numeric,SphericalDistribution-method}

\title{Spherical distribution class}
\description{
Class \code{SphericalDistribution} implements general spherically symmetric
distributions, i.e. starting from a random variable \code{L} distributed
according to a univariate distribution \code{radDistr} with positive support
serving as radial distribution, and an independent random variable \code{U} distributed
uniformly on the \code{dim} dimensional sphere, this is the
distribution of \code{LU}.
}
\section{Objects from the Class}{
  Objects could in principle be created by calls to \code{new}, but more
  frequently you would create them  via the generating function
  \code{\link{SphericalDistribution}}.
}
\section{Slots}{
  \describe{
    \item{\code{img}}{Object of class \code{"Reals"}. }
    \item{\code{param}}{Object of class \code{"SphericalParameter"}. }
    \item{\code{r}}{function with argument \code{n}; random number generator}
    \item{\code{d}}{optional function; in case it exists:
    the density of the distribution}
    \item{\code{p}}{optional function; in case it is non-null:
    the cdf of the distribution evaluated on rectangles, i.e. if a random
    variable \code{X} is distributed according to an \code{object} of class
    \code{"SphericalDistribution"},
    for \code{q} a matrix of dimension \eqn{d \times n}{d x n} \code{p(object)(q)}
    returns, for each of the \code{n} columns
    \eqn{P(X_i\leq q_i,\;i=1,\ldots,d)}{P(X[i]<= q[i],\;i=1,\dots,d)}.}
    \item{\code{q}}{optional function; in case it is non-null:
    the quantile of the distribution evaluated on rectangles, i.e. if a random
    variable \code{X} is distributed according to an \code{object} of class
    \code{"SphericalDistribution"},
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
    \item{\code{Symmetry}}{object of class \code{"SphericalSymmetry"} about
    center \code{loc}; used internally to avoid unnecessary calculations.}
}
}
\section{Extends}{
Class \code{"MultivariateDistribution"}, directly. \cr
Class \code{"Distribution"}, by class \code{"MultivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{dimension}{\code{signature(object = "SphericalDistribution")}: returns
    the dimension of the distribution. }
    \item{dim}{\code{signature(object = "SphericalDistribution")}: synonym to
    \code{dimension}. }
    \item{location}{\code{signature(object = "SphericalDistribution")}: helper
    function to have the same interface as class \code{"EllipticalDistribution"};
    always returns \code{0} (in the respective dimension). }
    \item{scale}{\code{signature(object = "SphericalDistribution")}: helper
    function to have the same interface as class \code{"EllipticalDistribution"};
    always returns the unit matrix  (in the respective dimension). }
    \item{radDistr}{\code{signature(object = "SphericalDistribution")}: access method for
      slot \code{radDistr}. }
    \item{r.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{r} of slot \code{radDistr}. }
    \item{d.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{d} of slot \code{radDistr}. }
    \item{p.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{p} of slot \code{radDistr}. }
    \item{q.rd}{\code{signature(object = "SphericalDistribution")}: wrapped access method for
      slot \code{q} of slot \code{radDistr}. }
    \item{plot.rd}{\code{signature(x = "SphericalDistribution")}: utility; calls \code{plot}
      for slot \code{radDistr}. }
    \item{plot}{\code{signature(x = "SphericalDistribution", y = "missing")}:
      plot for an spherically symmetric distribution; see \code{\link{plot-methods}}.
    }
    \item{show}{\code{signature(object = "SphericalDistribution")}: 
      \code{show} method for spherically symmetric distributions.
    }
    \item{showobj}{\code{signature(object = "SphericalDistribution")}: 
      \code{showobj} method for spherically symmetric distributions.
    }
    \item{E}{\code{signature(object = "SphericalDistribution", fun = "missing", cond = "missing")}:
      expectation of an elliptically symmetric distribution; exact.
    }
    \item{var}{\code{signature(x = "SphericalDistribution")}:
      expectation of an elliptically symmetric distribution; exact.
    }
    \item{coerce}{\code{signature(from = "SphericalDistribution", to = "EllipticalDistribution")}:
      create a \code{EllipticalDistribution} object from a spherically symmetric
      distribution. }
    \item{\code{+}}{\code{signature(e1 = "SphericalDistribution", e2 = "numeric")}:
      affine linear transformation; exact.
    }
    \item{\code{-}}{\code{signature(e1 = "SphericalDistribution", e2 = "numeric")}:
      affine linear transformation; exact.
    }
    \item{\code{-}}{\code{signature(e1 = "SphericalDistribution", e2 = "missing")}:
      affine linear transformation; exact.
    }
    \item{\code{*}}{\code{signature(e1 = "SphericalDistribution", e2 = "numeric")}:
      affine linear transformation; exact.
    }
    \item{\code{+}}{\code{signature(e1 = "numeric", e2 = "SphericalDistribution")}:
      affine linear transformation; exact.
    }
    \item{\code{-}}{\code{signature(e1 = "numeric", e2 = "SphericalDistribution")}:
      affine linear transformation; exact.
    }
    \item{\code{*}}{\code{signature(e1 = "numeric", e2 = "SphericalDistribution")}:
      affine linear transformation; exact.
    }
    \item{\code{\%*\%}}{\code{signature(e1 = "numeric", e2 = "SphericalDistribution")}:
      affine linear transformation; exact.
    }

  }
}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
new("SphericalDistribution") ## better use SphericalDistribution()
}
\concept{Spherical Distribution}
\keyword{distribution}
\concept{extreme value distribution}
\concept{absolutely continuous distribution}
\concept{S4 distribution class}
