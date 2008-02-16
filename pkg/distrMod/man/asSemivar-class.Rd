\name{asSemivar-class}
\docType{class}
\alias{asSemivar-class}

\title{Semivariance Risk Type}
\description{Class for semi-variance risk.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("asSemivar", ...)}.
  More frequently they are created via the generating function 
  \code{asSemivar}.   
}
\section{Slots}{
  \describe{
    \item{\code{sign}:}{Object of class \code{"numeric"} -- accounts for
     whether we only consider positive (sign = 1) or negative bias (sign = -1).}
  }
}
\section{Methods}{
No methods defined with class "asSemivar" in the signature.
}
\section{Extends}{
Class \code{"asGRisk"}, directly.\cr
}

\references{
  Ruckdeschel, P. (2005) Optimally One-Sided Bounded Influence Curves.
  Mathematical Methods in Statistics \emph{14}(1), 105-131.

  Kohl, M. (2005) \emph{Numerical Contributions to the Asymptotic Theory of Robustness}. 
  Bayreuth: Dissertation.
}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{}
\seealso{\code{\link{BiasType-class}}}
\examples{
asSemivar()
}
\concept{asymptotic risk}
\concept{semivariance}
\concept{bias}
\concept{risk}
\keyword{classes}
