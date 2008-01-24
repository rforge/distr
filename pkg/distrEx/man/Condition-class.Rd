\name{Condition-class}
\docType{class}
\alias{Condition-class}
\alias{name,Condition-method}
\alias{name<-,Condition-method}

\title{Conditions}
\description{The class of conditions.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Condition", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}: name of the condition}
  }
}
\section{Methods}{
\describe{
  \item{name}{\code{signature(object = "Condition")}: accessor function
    for slot \code{name}.}
  \item{name<-}{\code{signature(object = "Condition")}: replacement function
    for slot \code{name}.}
}
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{UnivariateCondDistribution-class}}}
\examples{new("Condition")}
\concept{conditioning}
\concept{condition}
\concept{S4 condition class}
\concept{conditional distribution}
\keyword{distribution}
