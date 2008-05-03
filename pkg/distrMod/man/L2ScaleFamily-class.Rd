\name{L2ScaleFamily-class}
\docType{class}
\alias{L2ScaleFamily-class}

\title{L2 differentiable parametric group family}
\description{Class of L2 differentiable parametric group families.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("L2ScaleFamily", ...)}.
  More frequently they are created via the generating function 
  \code{L2ScaleFamily}. 
}
\section{Slots}{
   \describe{
   \item{\code{name}:}{ object of class \code{"character"}: 
      name of the family. }
    \item{\code{distribution}:}{ object of class \code{"Distribution"}:
      member of the family. }
    \item{\code{distrSymm}:}{Object of class \code{"DistributionSymmetry"}: 
      symmetry of \code{distribution}. }
    \item{\code{param}:}{ object of class \code{"ParamFamParameter"}:
      parameter of the family. }
    \item{\code{modifyParam}:}{Object of class \code{"function"}:
      mapping from the parameter space (represented by \code{"param"}) 
      to the distribution space (represented by \code{"distribution"}). }
    \item{\code{props}:}{ object of class \code{"character"}:
      properties of the family. }
    \item{\code{L2deriv}:}{ object of class \code{"EuclRandVariable"}:
      L2 derivative of the family. }
    \item{\code{L2deriv.fct}:}{ object of class \code{"function"}: mapping from 
      the parameter space (argument \code{param} of class 
      \code{"ParamFamParameter"}) to a mapping from observation \code{x} to the  
      value of the L2derivative; \code{L2deriv.fct} is then used from observation
      \code{x} to value of the L2derivative; \code{L2deriv.fct} is used by 
      \code{modifyModel} to  move the L2deriv according to a change in the 
      parameter }
    \item{\code{L2derivSymm}:}{ object of class \code{"FunSymmList"}:
      symmetry of the maps included in \code{L2deriv}. }
    \item{\code{L2derivDistr}:}{ object of class \code{"UnivarDistrList"}:
      list which includes the distribution of \code{L2deriv}. }
    \item{\code{L2derivDistrSymm}:}{ object of class \code{"DistrSymmList"}:
      symmetry of the distributions included in \code{L2derivDistr}. }
    \item{\code{FisherInfo.fct}:}{ object of class \code{"function"}: 
      mapping from the parameter space (argument  \code{param} of class 
      \code{"ParamFamParameter"}) to the set of positive
      semidefinite matrices; \code{FisherInfo.fct} is used by \code{modifyModel} to 
      move the Fisher information according to a change in the parameter } 
    \item{\code{FisherInfo}:}{ object of class \code{"PosDefSymmMatrix"}:
      Fisher information of the family. }
    \item{\code{LogDeriv}:}{object of class \code{"function"}: has argument \code{x}; 
      the negative logarithmic derivative of the density of the model distribution at 
      the "standard" parameter value. }
}}
\section{Extends}{
Class \code{"L2GroupParamFamily"}, directly.\cr
Class \code{"L2ParamFamily"}, by class \code{"L2GroupParamFamily"}.\cr
Class \code{"ParamFamily"}, by class \code{"L2ParamFamily"}.\cr
Class \code{"ProbFamily"}, by class \code{"ParamFamily"}.
}
\section{Methods}{
[kommt noch] no specific methods implemented
}
\references{
  Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.

  Kohl, M. (2005) \emph{Numerical Contributions to the Asymptotic Theory of Robustness}. 
  Bayreuth: Dissertation.
}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@web.de}}
%\note{}
\seealso{\code{\link{L2ScaleFamily}}, \code{\link{ParamFamily-class}}}
\examples{
F1 <- new("L2ScaleFamily")
plot(F1)
}
\concept{parametric family}
\keyword{classes}
\keyword{models}
