\name{UnivariateCondDistribution-class}
\docType{class}
\alias{UnivariateCondDistribution-class}
\alias{cond}
\alias{cond,UnivariateCondDistribution-method}
\alias{plot,UnivariateCondDistribution-method}
\alias{show,UnivariateCondDistribution-method}

\title{Univariate conditional distribution}
\description{Class of univariate conditional distributions.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("UnivariateCondDistribution", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{cond}:}{Object of class \code{"Condition"}: condition }
    \item{\code{img}:}{Object of class \code{"rSpace"}: the image space. }
    \item{\code{param}:}{Object of class \code{"OptionalParameter"}: an 
      optional parameter.}
    \item{\code{r}:}{Object of class \code{"function"}: 
      generates random numbers. }
    \item{\code{d}:}{Object of class \code{"OptionalFunction"}: 
      optional conditional density function. }
    \item{\code{p}:}{Object of class \code{"OptionalFunction"}: 
      optional conditional cumulative distribution function. }
    \item{\code{q}:}{Object of class \code{"OptionalFunction"}:
      optional conditional quantile function. }
    \item{\code{.withArith}:}{logical: used internally to issue warnings as to 
            interpretation of arithmetics}
    \item{\code{.withSim}:}{logical: used internally to issue warnings as to 
          accuracy}
    \item{\code{.logExact}:}{logical: used internally to flag the case where 
    there are explicit formulae for the log version of density, cdf, and 
    quantile function}
    \item{\code{.lowerExact}:}{logical: used internally to flag the case where 
    there are explicit formulae for the lower tail version of cdf and quantile 
    function}
    \item{\code{Symmetry}:}{object of class \code{"DistributionSymmetry"};
     used internally to avoid unnecessary calculations.}
  }
}
\section{Extends}{
Class \code{"UnivariateDistribution"}, directly.\cr
Class \code{"Distribution"}, by class \code{"UnivariateDistribution"}.
}
\section{Methods}{
  \describe{
  \item{cond}{\code{signature(object = "UnivariateCondDistribution")}:
    accessor function for slot \code{cond}.}

  \item{show}{\code{signature(object = "UnivariateCondDistribution")}}

  \item{plot}{\code{signature(object = "UnivariateCondDistribution")}:
    not yet implemented.}
  }
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distr]{Distribution-class}}}
\examples{new("UnivariateCondDistribution")}
\concept{conditional distribution}
\keyword{distribution}
