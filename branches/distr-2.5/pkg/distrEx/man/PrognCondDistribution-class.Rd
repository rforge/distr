\name{PrognCondDistribution-class}
\docType{class}
\alias{PrognCondDistribution-class}

\title{Posterior distribution in convolution}
\description{The posterior distribution of X given (X+E)=y}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{\link{PrognCondDistribution}}
where \code{Regr} and \code{error} are the respective (a.c.) distributions of X and E
and the other arguments control accuracy in integration.
}
\section{Slots}{
  \describe{
    \item{\code{cond}:}{Object of class \code{"PrognCondition"}: condition }
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
    \item{\code{gaps}:}{(numeric) matrix or \code{NULL}}
    \item{\code{.withArith}:}{logical: used internally to issue warnings as to interpretation of arithmetics}
    \item{\code{.withSim}:}{logical: used internally to issue warnings as to accuracy}
    \item{\code{.logExact}:}{logical: used internally to flag the case where there are explicit formulae for the
                              log version of density, cdf, and quantile function}
    \item{\code{.lowerExact}:}{logical: used internally to flag the case where there are explicit formulae for the
                              lower tail version of cdf and quantile function}
  }
}
\section{Extends}{
Class \code{"AbscontCondDistribution"}, directly.\cr
Class \code{"Distribution"}, by classes \code{"UnivariateCondDistribution"} and \code{"AbscontCondDistribution"}.
}


%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{PrognCondition-class}}, \code{\link{UnivariateCondDistribution-class}}
\code{\link{AbscontCondDistribution-class}}, \code{\link[distr]{Distribution-class}}}
\examples{PrognCondDistribution()}
\concept{S4 distribution class}
\concept{conditional distribution}
\keyword{distribution}
