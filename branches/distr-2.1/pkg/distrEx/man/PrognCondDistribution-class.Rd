\name{PrognCondDistribution-class}
\docType{class}
\alias{PrognCondDistribution-class}
\alias{PrognCondDistribution}

\title{Posterior distribution in convolution}
\description{The posterior distribution of X given (X+E)=y}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{PrognCondDistribution(Regr, Error)}
where \code{Regr} and \code{error} are the respective (a.c.) distributions of X and E.
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
    \item{\code{.withArith}:}{logical: used internally to issue warnings as to interpretation of arithmetics}
    \item{\code{.withSim}:}{logical: used internally to issue warnings as to accuracy}
  }
}
\section{Extends}{
Class \code{"AbscontCondDistribution"}, directly.\cr
Class \code{"Distribution"}, by classes \code{"UnivariateCondDistribution"} and \code{"AbscontCondDistribution"}.
}
\usage{PrognCondDistribution(Regr = Norm(), Error = Norm())}
\arguments{
  \item{Regr}{ an object of class \code{"AbscontDistribution"} }
  \item{Error}{ an object of class \code{"AbscontDistribution"} }
  }
\details{For independent r.v.'s X,E with univariate, absolutely continuous 
 (a.c.) distributions \code{Regr} and \code{Error}, respectively, 
  \code{PrognCondDistribution()} returns the 
  (factorized, conditional) posterior distribution of X given X+E=y.
  as an object of class \code{PrognCondDistribution}.
}
\value{Object of class \code{"PrognCondDistribution"}}


%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{PrognCondition-class}}, \code{\link{UnivariateCondDistribution-class}}
\code{\link{AbscontCondDistribution-class}}, \code{\link[distr]{Distribution-class}}}
\examples{PrognCondDistribution()}
\concept{S4 distribution class}
\concept{conditional distribution}
\keyword{distribution}
