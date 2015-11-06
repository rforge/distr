\name{PrognCondition-class}
\docType{class}
\alias{PrognCondition-class}
\alias{show,PrognCondition-method}
\alias{PrognCondition}

\title{Conditions of class 'PrognCondition'}
\description{The class \code{PrognCondition} realizes the condition that X+E=y in a convolution setup}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{PrognCondition(range)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}}{Object of class \code{"character"}: name of the PrognCondition}
    \item{\code{range}}{Object of class \code{"EuclideanSpace"}: range of the PrognCondition}
  }
}
\section{Extends}{
Class \code{"Condition"}, directly.
}

\usage{PrognCondition(range = EuclideanSpace())}
\arguments{
  \item{range}{ an object of class \code{"EuclideanSpace"} }
  }
\value{Object of class \code{"PrognCondition"}}

\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "PrognCondition")}}
}
}

%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{PrognCondDistribution-class}},\code{\link{Condition-class}}}
\examples{PrognCondition()}
\concept{condition}
\concept{conditioning}
\concept{S4 condition class}
\concept{prediction distribution}
\concept{conditional distribution}
\keyword{distribution}
