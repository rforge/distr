\name{Estimator}
\alias{Estimator}

\title{ Function to compute estimates }
\description{
  The function \code{Estimator} provides a general way to compute 
  estimates.
}
\usage{
Estimator(x, estimator, name, Infos, asvar = NULL, ...)
}
\arguments{
  \item{x}{ (empirical) data }
  \item{estimator}{ function: estimator to be evaluated on \code{x}. }
  \item{name}{ optional name for estimator. }
  \item{Infos}{ character: optional informations about estimator }
  \item{asvar}{ OptionalMatrix: optionally the asymptotic (co)variance of the estimator }
  \item{\dots}{ further arguments to \code{estimator}. }
}
\details{
  The argument \code{criterion} has to be a function with arguments the 
  empirical data as well as an object of class \code{"Distribution"} 
  and possibly \code{\dots}.
}
\value{
  An object of S4-class \code{"Estimate"}.
}
%\references{  }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{Estimate-class}} }
\examples{
x <- rnorm(100)
Estimator(x, estimator = mean, name = "mean")

X <- matrix(rnorm(1000), nrow = 10)
Estimator(X, estimator = rowMeans, name = "mean")
}
\keyword{univar}
